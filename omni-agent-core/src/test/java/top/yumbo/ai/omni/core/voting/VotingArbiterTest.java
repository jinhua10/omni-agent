package top.yumbo.ai.omni.core.voting;

import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.DisplayName;
import top.yumbo.ai.voting.api.model.Vote;
import top.yumbo.ai.voting.api.model.VoterType;
import top.yumbo.ai.voting.api.model.VotingResult;
import top.yumbo.ai.voting.api.model.VotingSession;

import java.util.List;
import java.util.Map;

import static org.junit.jupiter.api.Assertions.*;

/**
 * VotingArbiter Unit Test
 *
 * @author OmniAgent Team
 * @since 1.0.0
 */
@DisplayName("Voting Arbiter Test")
class VotingArbiterTest {

    private VotingArbiter votingArbiter;

    @BeforeEach
    void setUp() {
        votingArbiter = new VotingArbiter();
    }

    @Test
    @DisplayName("Create voting session")
    void testCreateSession() {
        VotingSession session = votingArbiter.createSession(
            "New Concept", "Description", "user1", 60, 3, 0.6);

        assertNotNull(session);
        assertNotNull(session.getSessionId());
        assertEquals("New Concept", session.getTopic());
        assertEquals(VotingSession.VotingStatus.OPEN, session.getStatus());
    }

    @Test
    @DisplayName("Cast approve vote")
    void testCastApproveVote() {
        VotingSession session = votingArbiter.createSession(
            "Topic", "Desc", "initiator", 60, 1, 0.5);

        Vote vote = Vote.builder()
            .sessionId(session.getSessionId())
            .voterId("voter1")
            .voterType(VoterType.USER)
            .choice(Vote.VoteChoice.APPROVE)
            .build();

        Vote castVote = votingArbiter.castVote(vote);

        assertNotNull(castVote);
        assertNotNull(castVote.getVoteId());
        assertEquals(Vote.VoteChoice.APPROVE, castVote.getChoice());
    }

    @Test
    @DisplayName("Get votes for session")
    void testGetVotes() {
        VotingSession session = votingArbiter.createSession(
            "Topic", "Desc", "initiator", 60, 1, 0.5);

        votingArbiter.castVote(createVote(session.getSessionId(), "v1", Vote.VoteChoice.APPROVE));
        votingArbiter.castVote(createVote(session.getSessionId(), "v2", Vote.VoteChoice.REJECT));

        List<Vote> votes = votingArbiter.getVotes(session.getSessionId());

        assertEquals(2, votes.size());
    }

    @Test
    @DisplayName("Calculate voting result")
    void testCalculateResult() {
        VotingSession session = votingArbiter.createSession(
            "Topic", "Desc", "initiator", 60, 1, 0.5);

        votingArbiter.castVote(createVote(session.getSessionId(), "v1", Vote.VoteChoice.APPROVE));
        votingArbiter.castVote(createVote(session.getSessionId(), "v2", Vote.VoteChoice.APPROVE));
        votingArbiter.castVote(createVote(session.getSessionId(), "v3", Vote.VoteChoice.REJECT));

        VotingResult result = votingArbiter.calculateResult(session.getSessionId());

        assertNotNull(result);
        assertEquals(2, result.getApproveCount());
        assertEquals(1, result.getRejectCount());
        assertEquals(3, result.getTotalParticipants());
    }

    @Test
    @DisplayName("Arbitrate - approved")
    void testArbitrateApproved() {
        VotingSession session = votingArbiter.createSession(
            "Topic", "Desc", "initiator", 60, 3, 0.6);

        votingArbiter.castVote(createVote(session.getSessionId(), "v1", Vote.VoteChoice.APPROVE));
        votingArbiter.castVote(createVote(session.getSessionId(), "v2", Vote.VoteChoice.APPROVE));
        votingArbiter.castVote(createVote(session.getSessionId(), "v3", Vote.VoteChoice.APPROVE));
        votingArbiter.castVote(createVote(session.getSessionId(), "v4", Vote.VoteChoice.REJECT));

        VotingResult result = votingArbiter.arbitrate(session.getSessionId());

        assertEquals(VotingResult.Decision.APPROVED, result.getFinalDecision());
    }

    @Test
    @DisplayName("Arbitrate - rejected")
    void testArbitrateRejected() {
        VotingSession session = votingArbiter.createSession(
            "Topic", "Desc", "initiator", 60, 3, 0.7);

        votingArbiter.castVote(createVote(session.getSessionId(), "v1", Vote.VoteChoice.APPROVE));
        votingArbiter.castVote(createVote(session.getSessionId(), "v2", Vote.VoteChoice.APPROVE));
        votingArbiter.castVote(createVote(session.getSessionId(), "v3", Vote.VoteChoice.REJECT));
        votingArbiter.castVote(createVote(session.getSessionId(), "v4", Vote.VoteChoice.REJECT));

        VotingResult result = votingArbiter.arbitrate(session.getSessionId());

        assertEquals(VotingResult.Decision.REJECTED, result.getFinalDecision());
    }

    @Test
    @DisplayName("Arbitrate - insufficient participants")
    void testArbitrateInsufficientParticipants() {
        VotingSession session = votingArbiter.createSession(
            "Topic", "Desc", "initiator", 60, 5, 0.6);

        votingArbiter.castVote(createVote(session.getSessionId(), "v1", Vote.VoteChoice.APPROVE));
        votingArbiter.castVote(createVote(session.getSessionId(), "v2", Vote.VoteChoice.APPROVE));

        VotingResult result = votingArbiter.arbitrate(session.getSessionId());

        assertEquals(VotingResult.Decision.PENDING, result.getFinalDecision());
    }

    @Test
    @DisplayName("Revoke vote")
    void testRevokeVote() {
        VotingSession session = votingArbiter.createSession(
            "Topic", "Desc", "initiator", 60, 1, 0.5);
        Vote vote = votingArbiter.castVote(
            createVote(session.getSessionId(), "voter", Vote.VoteChoice.APPROVE));

        boolean revoked = votingArbiter.revokeVote(vote.getVoteId());

        assertTrue(revoked);
        assertEquals(0, votingArbiter.getVotes(session.getSessionId()).size());
    }

    @Test
    @DisplayName("Close session")
    void testCloseSession() {
        VotingSession session = votingArbiter.createSession(
            "Topic", "Desc", "initiator", 60, 1, 0.5);
        votingArbiter.castVote(createVote(session.getSessionId(), "v", Vote.VoteChoice.APPROVE));

        boolean closed = votingArbiter.closeSession(session.getSessionId());

        assertTrue(closed);
    }

    @Test
    @DisplayName("Get statistics")
    void testGetStatistics() {
        votingArbiter.createSession("T1", "D", "i", 60, 1, 0.5);
        votingArbiter.createSession("T2", "D", "i", 60, 1, 0.5);

        Map<String, Object> stats = votingArbiter.getStatistics();

        assertNotNull(stats);
        assertTrue(stats.containsKey("totalSessions"));
    }

    private Vote createVote(String sessionId, String voterId, Vote.VoteChoice choice) {
        return Vote.builder()
            .sessionId(sessionId)
            .voterId(voterId)
            .voterType(VoterType.USER)
            .choice(choice)
            .build();
    }
}

