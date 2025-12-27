package top.yumbo.ai.omni.voting.api;

import top.yumbo.ai.omni.voting.api.model.Vote;
import top.yumbo.ai.omni.voting.api.model.VotingResult;
import top.yumbo.ai.omni.voting.api.model.VotingSession;

import java.util.List;
import java.util.Optional;

/**
 * 投票服务接口
 * (Voting Service Interface)
 *
 * <p>知识冲突的民主化解决机制</p>
 * <p>支持多角色加权投票</p>
 *
 * @author OmniAgent Team
 * @since 1.0.0
 */
public interface VotingService {

    // ========== 投票会话管理 ==========

    /**
     * 创建投票会话
     *
     * @param topic 议题
     * @param description 描述
     * @param initiatorId 发起者ID
     * @param durationMinutes 持续时长（分钟）
     * @param minParticipants 最小参与人数
     * @param approvalThreshold 通过阈值（0-1）
     * @return 投票会话
     */
    VotingSession createSession(String topic, String description, String initiatorId,
                                int durationMinutes, int minParticipants, double approvalThreshold);

    /**
     * 获取投票会话
     *
     * @param sessionId 会话ID
     * @return 投票会话
     */
    Optional<VotingSession> getSession(String sessionId);

    /**
     * 获取所有进行中的会话
     *
     * @return 会话列表
     */
    List<VotingSession> getActiveSessions();

    /**
     * 关闭投票会话
     *
     * @param sessionId 会话ID
     * @return 是否成功
     */
    boolean closeSession(String sessionId);

    // ========== 投票操作 ==========

    /**
     * 投票
     *
     * @param vote 投票对象
     * @return 投票记录
     */
    Vote castVote(Vote vote);

    /**
     * 获取会话的所有投票
     *
     * @param sessionId 会话ID
     * @return 投票列表
     */
    List<Vote> getVotes(String sessionId);

    /**
     * 撤回投票
     *
     * @param voteId 投票ID
     * @return 是否成功
     */
    boolean revokeVote(String voteId);

    // ========== 结果计算 ==========

    /**
     * 计算投票结果
     *
     * @param sessionId 会话ID
     * @return 投票结果
     */
    VotingResult calculateResult(String sessionId);

    /**
     * 自动仲裁
     *
     * @param sessionId 会话ID
     * @return 投票结果
     */
    VotingResult arbitrate(String sessionId);

    /**
     * 获取统计信息
     *
     * @return 统计数据
     */
    java.util.Map<String, Object> getStatistics();
}

