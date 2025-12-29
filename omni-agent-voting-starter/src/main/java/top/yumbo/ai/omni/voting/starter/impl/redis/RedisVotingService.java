package top.yumbo.ai.omni.voting.starter.impl.redis;

import lombok.extern.slf4j.Slf4j;
import org.springframework.data.redis.core.RedisTemplate;
import top.yumbo.ai.omni.voting.api.VotingService;
import top.yumbo.ai.omni.voting.api.model.Vote;
import top.yumbo.ai.omni.voting.api.model.VotingResult;
import top.yumbo.ai.omni.voting.api.model.VotingSession;

import java.time.LocalDateTime;
import java.util.*;
import java.util.concurrent.TimeUnit;
import java.util.stream.Collectors;

/**
 * Redis投票服务实现
 * (Redis Voting Service Implementation)
 *
 * 基于Redis的分布式投票仲裁实现
 * (Redis-based distributed voting arbitration implementation)
 *
 * @author OmniAgent Team
 * @since 1.0.0
 */
@Slf4j
public class RedisVotingService implements VotingService {

    private final RedisTemplate<String, Object> redisTemplate;
    private final RedisVotingProperties properties;

    // Redis Key前缀
    private static final String SESSION_PREFIX = "session:";
    private static final String VOTE_PREFIX = "vote:";
    private static final String SESSION_VOTES_PREFIX = "session:votes:";

    public RedisVotingService(RedisTemplate<String, Object> redisTemplate,
                             RedisVotingProperties properties) {
        this.redisTemplate = redisTemplate;
        this.properties = properties;
        log.info("RedisVotingService initialized with Redis: {}:{}",
                properties.getHost(), properties.getPort());
    }

    @Override
    public VotingSession createSession(String topic, String description, String initiatorId,
                                      int durationMinutes, int minParticipants, double approvalThreshold) {
        String sessionId = UUID.randomUUID().toString();

        VotingSession session = VotingSession.builder()
                .sessionId(sessionId)
                .topic(topic)
                .description(description)
                .initiatorId(initiatorId)
                .createTime(LocalDateTime.now())
                .deadline(LocalDateTime.now().plusMinutes(durationMinutes))
                .status(VotingSession.VotingStatus.OPEN)
                .minParticipants(minParticipants)
                .approvalThreshold(approvalThreshold)
                .votes(new ArrayList<>())
                .build();

        // 存储会话
        String sessionKey = getKey(SESSION_PREFIX + sessionId);
        redisTemplate.opsForValue().set(sessionKey, session,
                durationMinutes, TimeUnit.MINUTES);

        // 初始化投票集合
        String votesKey = getKey(SESSION_VOTES_PREFIX + sessionId);
        redisTemplate.opsForSet().add(votesKey, "init");
        redisTemplate.expire(votesKey, durationMinutes, TimeUnit.MINUTES);

        log.info("Created voting session: {} for topic: {}", sessionId, topic);

        return session;
    }

    @Override
    public Optional<VotingSession> getSession(String sessionId) {
        String sessionKey = getKey(SESSION_PREFIX + sessionId);
        VotingSession session = (VotingSession) redisTemplate.opsForValue().get(sessionKey);
        return Optional.ofNullable(session);
    }

    @Override
    public List<VotingSession> getActiveSessions() {
        Set<String> keys = redisTemplate.keys(getKey(SESSION_PREFIX + "*"));
        if (keys == null) return Collections.emptyList();

        return keys.stream()
                .map(key -> (VotingSession) redisTemplate.opsForValue().get(key))
                .filter(Objects::nonNull)
                .filter(s -> s.getStatus() == VotingSession.VotingStatus.OPEN)
                .collect(Collectors.toList());
    }

    @Override
    public boolean closeSession(String sessionId) {
        String sessionKey = getKey(SESSION_PREFIX + sessionId);
        VotingSession session = (VotingSession) redisTemplate.opsForValue().get(sessionKey);

        if (session == null) return false;

        session.setStatus(VotingSession.VotingStatus.CLOSED);
        VotingResult result = arbitrate(sessionId);
        session.setResult(result);

        if (result.getFinalDecision() == VotingResult.Decision.APPROVED) {
            session.setStatus(VotingSession.VotingStatus.APPROVED);
        } else if (result.getFinalDecision() == VotingResult.Decision.REJECTED) {
            session.setStatus(VotingSession.VotingStatus.REJECTED);
        }

        redisTemplate.opsForValue().set(sessionKey, session,
                properties.getSessionExpirationMinutes(), TimeUnit.MINUTES);

        log.info("Closed session: {} - {}", sessionId, result.getFinalDecision());
        return true;
    }

    @Override
    public Vote castVote(Vote vote) {
        String voteId = UUID.randomUUID().toString();
        vote.setVoteId(voteId);
        vote.setVoteTime(LocalDateTime.now());

        if (vote.getWeight() == 0) {
            vote.setWeight(vote.getVoterType().getDefaultWeight());
        }

        // 存储投票
        String voteKey = getKey(VOTE_PREFIX + voteId);
        redisTemplate.opsForValue().set(voteKey, vote,
                properties.getSessionExpirationMinutes(), TimeUnit.MINUTES);

        // 添加到会话的投票集合
        String votesKey = getKey(SESSION_VOTES_PREFIX + vote.getSessionId());
        redisTemplate.opsForSet().add(votesKey, voteId);

        log.info("Vote cast: {} - {}", voteId, vote.getChoice());
        return vote;
    }

    @Override
    public List<Vote> getVotes(String sessionId) {
        String votesKey = getKey(SESSION_VOTES_PREFIX + sessionId);
        Set<Object> voteIds = redisTemplate.opsForSet().members(votesKey);

        if (voteIds == null) return Collections.emptyList();

        return voteIds.stream()
                .filter(id -> !"init".equals(id))
                .map(Object::toString)
                .map(id -> (Vote) redisTemplate.opsForValue().get(getKey(VOTE_PREFIX + id)))
                .filter(Objects::nonNull)
                .collect(Collectors.toList());
    }

    @Override
    public boolean revokeVote(String voteId) {
        String voteKey = getKey(VOTE_PREFIX + voteId);
        Vote vote = (Vote) redisTemplate.opsForValue().get(voteKey);

        if (vote != null) {
            String votesKey = getKey(SESSION_VOTES_PREFIX + vote.getSessionId());
            redisTemplate.opsForSet().remove(votesKey, voteId);
            redisTemplate.delete(voteKey);
            log.info("Vote revoked: {}", voteId);
            return true;
        }
        return false;
    }

    @Override
    public VotingResult calculateResult(String sessionId) {
        List<Vote> voteList = getVotes(sessionId);

        int approveCount = 0, rejectCount = 0, abstainCount = 0;
        double approveScore = 0, rejectScore = 0, abstainScore = 0;

        for (Vote vote : voteList) {
            switch (vote.getChoice()) {
                case APPROVE -> {
                    approveCount++;
                    approveScore += vote.getWeight();
                }
                case REJECT -> {
                    rejectCount++;
                    rejectScore += vote.getWeight();
                }
                case ABSTAIN -> {
                    abstainCount++;
                    abstainScore += vote.getWeight();
                }
            }
        }

        double totalScore = approveScore + rejectScore;
        double approvalRate = totalScore > 0 ? (approveScore / totalScore) * 100 : 0;

        return VotingResult.builder()
                .sessionId(sessionId)
                .approveCount(approveCount)
                .rejectCount(rejectCount)
                .abstainCount(abstainCount)
                .approveScore(approveScore)
                .rejectScore(rejectScore)
                .abstainScore(abstainScore)
                .totalParticipants(voteList.size())
                .approvalRate(approvalRate)
                .build();
    }

    @Override
    public VotingResult arbitrate(String sessionId) {
        String sessionKey = getKey(SESSION_PREFIX + sessionId);
        VotingSession session = (VotingSession) redisTemplate.opsForValue().get(sessionKey);

        if (session == null) {
            throw new IllegalArgumentException("Session not found: " + sessionId);
        }

        VotingResult result = calculateResult(sessionId);

        if (result.getTotalParticipants() < session.getMinParticipants()) {
            result.setFinalDecision(VotingResult.Decision.PENDING);
            result.setReason("Insufficient participants");
        } else if (result.getApprovalRate() >= session.getApprovalThreshold() * 100) {
            result.setFinalDecision(VotingResult.Decision.APPROVED);
            result.setReason(String.format("Approved: %.1f%%", result.getApprovalRate()));
        } else {
            result.setFinalDecision(VotingResult.Decision.REJECTED);
            result.setReason(String.format("Rejected: %.1f%%", result.getApprovalRate()));
        }

        log.info("Arbitrated session: {} - {} ({})", 
                sessionId, result.getFinalDecision(), result.getReason());

        return result;
    }

    @Override
    public Map<String, Object> getStatistics() {
        Set<String> sessionKeys = redisTemplate.keys(getKey(SESSION_PREFIX + "*"));
        Set<String> voteKeys = redisTemplate.keys(getKey(VOTE_PREFIX + "*"));

        int totalSessions = (sessionKeys != null) ? sessionKeys.size() : 0;
        int totalVotes = (voteKeys != null) ? voteKeys.size() : 0;

        Map<String, Object> stats = new HashMap<>();
        stats.put("totalSessions", totalSessions);
        stats.put("activeSessions", getActiveSessions().size());
        stats.put("totalVotes", totalVotes);
        stats.put("storage_backend", "redis");

        return stats;
    }

    // ========== 辅助方法 ==========

    private String getKey(String suffix) {
        return properties.getKeyPrefix() + suffix;
    }
}
