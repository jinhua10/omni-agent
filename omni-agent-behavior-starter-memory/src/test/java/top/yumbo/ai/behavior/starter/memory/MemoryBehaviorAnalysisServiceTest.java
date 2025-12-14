package top.yumbo.ai.behavior.starter.memory;

import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import top.yumbo.ai.behavior.api.model.*;

import java.util.List;
import java.util.Map;

import static org.junit.jupiter.api.Assertions.*;

/**
 * MemoryBehaviorAnalysisService 单元测试
 *
 * @author OmniAgent Team
 * @since 1.0.0
 */
class MemoryBehaviorAnalysisServiceTest {

    private MemoryBehaviorAnalysisService service;

    @BeforeEach
    void setUp() {
        service = new MemoryBehaviorAnalysisService();
    }

    @Test
    void testCollectSignal() {
        // Given
        BehaviorSignalEvent signal = new BehaviorSignalEvent(
            "user1", "qa1", "answer1", SignalType.LIKE
        );

        // When
        service.collectSignal(signal);

        // Then
        List<BehaviorSignalEvent> signals = service.getUserAnswerSignals("user1", "answer1");
        assertEquals(1, signals.size());
        assertEquals(SignalType.LIKE, signals.getFirst().getSignalType());
    }

    @Test
    void testCollectSignalWithNullShouldNotFail() {
        // When & Then - should not throw exception
        assertDoesNotThrow(() -> service.collectSignal(null));
    }

    @Test
    void testCollectSignals() {
        // Given
        List<BehaviorSignalEvent> signals = List.of(
            new BehaviorSignalEvent("user1", "qa1", "answer1", SignalType.VIEW),
            new BehaviorSignalEvent("user1", "qa1", "answer1", SignalType.DWELL),
            new BehaviorSignalEvent("user1", "qa1", "answer1", SignalType.LIKE)
        );

        // When
        service.collectSignals(signals);

        // Then
        List<BehaviorSignalEvent> collected = service.getUserAnswerSignals("user1", "answer1");
        assertEquals(3, collected.size());
    }

    @Test
    void testInferAttitude_NoSignals() {
        // When
        AttitudeScore score = service.inferAttitude("user1", "answer1");

        // Then
        assertNotNull(score);
        assertEquals(0.0, score.getRawScore());
        assertEquals(0.0, score.getConfidence());
        assertEquals(AttitudeLevel.NEUTRAL, score.getLevel());
    }

    @Test
    void testInferAttitude_PositiveSignals() {
        // Given
        service.collectSignal(new BehaviorSignalEvent("user1", "qa1", "answer1", SignalType.LIKE));
        service.collectSignal(new BehaviorSignalEvent("user1", "qa1", "answer1", SignalType.COPY));
        service.collectSignal(new BehaviorSignalEvent("user1", "qa1", "answer1", SignalType.SHARE));

        // When
        AttitudeScore score = service.inferAttitude("user1", "answer1");

        // Then
        assertNotNull(score);
        assertTrue(score.getRawScore() > 0, "Expected positive score");
        assertTrue(score.isPositive());
        assertFalse(score.isNegative());
        assertTrue(score.getConfidence() > 0);
        assertEquals(3, score.getSupportingSignals().size());
    }

    @Test
    void testInferAttitude_NegativeSignals() {
        // Given
        service.collectSignal(new BehaviorSignalEvent("user1", "qa1", "answer1", SignalType.DISLIKE));

        // When
        AttitudeScore score = service.inferAttitude("user1", "answer1");

        // Then
        assertNotNull(score);
        assertTrue(score.getRawScore() < 0, "Expected negative score");
        assertTrue(score.isNegative());
        assertFalse(score.isPositive());
    }

    @Test
    void testInferAttitude_MixedSignals() {
        // Given
        service.collectSignal(new BehaviorSignalEvent("user1", "qa1", "answer1", SignalType.LIKE));
        service.collectSignal(new BehaviorSignalEvent("user1", "qa1", "answer1", SignalType.DISLIKE));
        service.collectSignal(new BehaviorSignalEvent("user1", "qa1", "answer1", SignalType.VIEW));

        // When
        AttitudeScore score = service.inferAttitude("user1", "answer1");

        // Then
        assertNotNull(score);
        // Score should be close to neutral due to mixed signals
        assertTrue(Math.abs(score.getRawScore()) < 1.0);
    }

    @Test
    void testInferAttitude_Caching() {
        // Given
        service.collectSignal(new BehaviorSignalEvent("user1", "qa1", "answer1", SignalType.LIKE));

        // When - first call
        AttitudeScore score1 = service.inferAttitude("user1", "answer1");
        // When - second call (should be cached)
        AttitudeScore score2 = service.inferAttitude("user1", "answer1");

        // Then
        assertNotNull(score1);
        assertNotNull(score2);
        assertEquals(score1.getRawScore(), score2.getRawScore());
    }

    @Test
    void testInferAttitudes() {
        // Given
        service.collectSignal(new BehaviorSignalEvent("user1", "qa1", "answer1", SignalType.LIKE));
        service.collectSignal(new BehaviorSignalEvent("user1", "qa2", "answer2", SignalType.DISLIKE));

        // When
        Map<String, AttitudeScore> scores = service.inferAttitudes("user1", List.of("answer1", "answer2"));

        // Then
        assertEquals(2, scores.size());
        assertTrue(scores.get("answer1").isPositive());
        assertTrue(scores.get("answer2").isNegative());
    }

    @Test
    void testGetUserSignals() {
        // Given
        service.collectSignal(new BehaviorSignalEvent("user1", "qa1", "answer1", SignalType.LIKE));
        service.collectSignal(new BehaviorSignalEvent("user1", "qa2", "answer2", SignalType.VIEW));

        // When
        List<BehaviorSignalEvent> signals = service.getUserSignals("user1");

        // Then
        assertEquals(2, signals.size());
    }

    @Test
    void testGetAnswerSignals() {
        // Given
        service.collectSignal(new BehaviorSignalEvent("user1", "qa1", "answer1", SignalType.LIKE));
        service.collectSignal(new BehaviorSignalEvent("user2", "qa2", "answer1", SignalType.VIEW));

        // When
        List<BehaviorSignalEvent> signals = service.getAnswerSignals("answer1");

        // Then
        assertEquals(2, signals.size());
    }

    @Test
    void testCalculateHotness() {
        // Given
        service.collectSignal(new BehaviorSignalEvent("user1", "qa1", "answer1", SignalType.LIKE));
        service.collectSignal(new BehaviorSignalEvent("user2", "qa2", "answer1", SignalType.LIKE));
        service.collectSignal(new BehaviorSignalEvent("user3", "qa3", "answer1", SignalType.SHARE));

        // When
        double hotness = service.calculateHotness("answer1");

        // Then
        assertTrue(hotness > 0);
        assertTrue(hotness <= 1.0);
    }

    @Test
    void testCalculateHotness_NoSignals() {
        // When
        double hotness = service.calculateHotness("answer1");

        // Then
        assertEquals(0.0, hotness);
    }

    @Test
    void testGetHotAnswers() {
        // Given
        // answer1 has more signals
        service.collectSignal(new BehaviorSignalEvent("user1", "qa1", "answer1", SignalType.LIKE));
        service.collectSignal(new BehaviorSignalEvent("user2", "qa2", "answer1", SignalType.LIKE));
        service.collectSignal(new BehaviorSignalEvent("user3", "qa3", "answer1", SignalType.SHARE));

        // answer2 has fewer signals
        service.collectSignal(new BehaviorSignalEvent("user4", "qa4", "answer2", SignalType.VIEW));

        // When
        List<String> hotAnswers = service.getHotAnswers(2);

        // Then
        assertEquals(2, hotAnswers.size());
        assertEquals("answer1", hotAnswers.getFirst()); // answer1 should be first (hottest)
    }

    @Test
    void testClearUserSignals() {
        // Given
        service.collectSignal(new BehaviorSignalEvent("user1", "qa1", "answer1", SignalType.LIKE));
        service.collectSignal(new BehaviorSignalEvent("user1", "qa2", "answer2", SignalType.VIEW));

        // When
        service.clearUserSignals("user1");

        // Then
        assertTrue(service.getUserSignals("user1").isEmpty());
    }

    @Test
    void testClearAnswerSignals() {
        // Given
        service.collectSignal(new BehaviorSignalEvent("user1", "qa1", "answer1", SignalType.LIKE));
        service.collectSignal(new BehaviorSignalEvent("user2", "qa2", "answer1", SignalType.VIEW));

        // When
        service.clearAnswerSignals("answer1");

        // Then
        assertTrue(service.getAnswerSignals("answer1").isEmpty());
    }

    @Test
    void testSignalStrength() {
        // Given
        BehaviorSignalEvent signal = new BehaviorSignalEvent("user1", "qa1", "answer1", SignalType.DWELL);
        signal.setStrength(0.5);

        // When
        service.collectSignal(signal);
        AttitudeScore score = service.inferAttitude("user1", "answer1");

        // Then
        assertNotNull(score);
        assertTrue(score.getRawScore() > 0);
    }

    @Test
    void testGetStatistics() {
        // Given
        service.collectSignal(new BehaviorSignalEvent("user1", "qa1", "answer1", SignalType.LIKE));
        service.collectSignal(new BehaviorSignalEvent("user2", "qa2", "answer2", SignalType.VIEW));

        // When
        Map<String, Object> stats = service.getStatistics();

        // Then
        assertNotNull(stats);
        assertEquals(2, stats.get("totalUsers"));
        assertEquals(2, stats.get("totalAnswers"));
        assertEquals(2, stats.get("totalSignals"));
    }

    @Test
    void testAttitudeLevel_VerySatisfied() {
        // Given - many positive signals
        for (int i = 0; i < 5; i++) {
            service.collectSignal(new BehaviorSignalEvent("user1", "qa1", "answer1", SignalType.LIKE));
        }

        // When
        AttitudeScore score = service.inferAttitude("user1", "answer1");

        // Then
        assertTrue(score.getLevel() == AttitudeLevel.VERY_SATISFIED ||
                   score.getLevel() == AttitudeLevel.SATISFIED);
    }

    @Test
    void testAttitudeLevel_VeryDissatisfied() {
        // Given - multiple negative signals
        for (int i = 0; i < 3; i++) {
            service.collectSignal(new BehaviorSignalEvent("user1", "qa1", "answer1", SignalType.DISLIKE));
        }

        // When
        AttitudeScore score = service.inferAttitude("user1", "answer1");

        // Then
        assertTrue(score.getLevel() == AttitudeLevel.VERY_DISSATISFIED ||
                   score.getLevel() == AttitudeLevel.DISSATISFIED);
    }

    @Test
    void testConcurrentAccess() throws InterruptedException {
        // Given
        Thread t1 = new Thread(() -> {
            for (int i = 0; i < 100; i++) {
                service.collectSignal(new BehaviorSignalEvent("user1", "qa1", "answer1", SignalType.LIKE));
            }
        });

        Thread t2 = new Thread(() -> {
            for (int i = 0; i < 100; i++) {
                service.collectSignal(new BehaviorSignalEvent("user2", "qa2", "answer2", SignalType.VIEW));
            }
        });

        // When
        t1.start();
        t2.start();
        t1.join();
        t2.join();

        // Then
        assertEquals(100, service.getUserSignals("user1").size());
        assertEquals(100, service.getUserSignals("user2").size());
    }
}

