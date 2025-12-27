package top.yumbo.ai.omni.persistence.api;

import lombok.extern.slf4j.Slf4j;
import top.yumbo.ai.omni.persistence.api.model.QuestionTypeConfig;

import java.util.*;
import java.util.concurrent.CompletableFuture;
import java.util.concurrent.ExecutorService;
import java.util.concurrent.Executors;

/**
 * 组合持久化实现 - 支持多后端异步写入
 * (Composite Persistence - Support Multiple Backend Async Writes)
 *
 * <p>
 * 特性 (Features):
 * - 支持多个持久化后端同时写入
 * - 异步并发写入，提升性能
 * - 主后端读取，其他后端作为备份
 * - 写入失败不影响主流程
 * </p>
 *
 * @author OmniAgent Team
 * @since 1.0.0
 */
@Slf4j
public class CompositePersistence implements QuestionClassifierPersistence {

    /**
     * 主持久化后端（用于读取）
     */
    private final QuestionClassifierPersistence primary;

    /**
     * 次要持久化后端列表（用于备份）
     */
    private final List<QuestionClassifierPersistence> secondaries;

    /**
     * 异步写入线程池
     */
    private final ExecutorService executorService;

    /**
     * 构造函数
     *
     * @param primary     主持久化后端
     * @param secondaries 次要持久化后端
     */
    public CompositePersistence(QuestionClassifierPersistence primary,
                                List<QuestionClassifierPersistence> secondaries) {
        this.primary = primary;
        this.secondaries = secondaries != null ? secondaries : Collections.emptyList();
        this.executorService = Executors.newFixedThreadPool(
            Math.max(2, this.secondaries.size()),
            r -> {
                Thread t = new Thread(r, "composite-persistence-worker");
                t.setDaemon(true);
                return t;
            }
        );

        log.info("CompositePersistence initialized with 1 primary + {} secondary backends",
            this.secondaries.size());
    }

    /**
     * 异步写入到次要后端
     */
    private void asyncWriteToSecondaries(Runnable writeOperation, String operationName) {
        if (secondaries.isEmpty()) {
            return;
        }

        CompletableFuture.runAsync(() -> {
            try {
                writeOperation.run();
                log.debug("{} succeeded on secondary backends", operationName);
            } catch (Exception e) {
                log.warn("{} failed on secondary backends: {}", operationName, e.getMessage());
            }
        }, executorService);
    }

    // ========== 问题类型管理 ==========

    @Override
    public boolean saveQuestionType(QuestionTypeConfig config) {
        // 同步写入主后端
        boolean result = primary.saveQuestionType(config);

        // 异步写入次要后端
        asyncWriteToSecondaries(
            () -> secondaries.forEach(p -> p.saveQuestionType(config)),
            "saveQuestionType"
        );

        return result;
    }

    @Override
    public int saveQuestionTypes(List<QuestionTypeConfig> configs) {
        // 同步写入主后端
        int result = primary.saveQuestionTypes(configs);

        // 异步写入次要后端
        asyncWriteToSecondaries(
            () -> secondaries.forEach(p -> p.saveQuestionTypes(configs)),
            "saveQuestionTypes"
        );

        return result;
    }

    @Override
    public Optional<QuestionTypeConfig> getQuestionType(String typeId) {
        // 只从主后端读取
        return primary.getQuestionType(typeId);
    }

    @Override
    public List<QuestionTypeConfig> getAllQuestionTypes() {
        // 只从主后端读取
        return primary.getAllQuestionTypes();
    }

    @Override
    public boolean updateQuestionType(QuestionTypeConfig config) {
        boolean result = primary.updateQuestionType(config);

        asyncWriteToSecondaries(
            () -> secondaries.forEach(p -> p.updateQuestionType(config)),
            "updateQuestionType"
        );

        return result;
    }

    @Override
    public boolean deleteQuestionType(String typeId) {
        boolean result = primary.deleteQuestionType(typeId);

        asyncWriteToSecondaries(
            () -> secondaries.forEach(p -> p.deleteQuestionType(typeId)),
            "deleteQuestionType"
        );

        return result;
    }

    // ========== 关键词管理 ==========

    @Override
    public boolean saveKeywords(String typeId, List<String> keywords) {
        boolean result = primary.saveKeywords(typeId, keywords);

        asyncWriteToSecondaries(
            () -> secondaries.forEach(p -> p.saveKeywords(typeId, keywords)),
            "saveKeywords"
        );

        return result;
    }

    @Override
    public boolean addKeywords(String typeId, List<String> keywords) {
        boolean result = primary.addKeywords(typeId, keywords);

        asyncWriteToSecondaries(
            () -> secondaries.forEach(p -> p.addKeywords(typeId, keywords)),
            "addKeywords"
        );

        return result;
    }

    @Override
    public List<String> getKeywords(String typeId) {
        return primary.getKeywords(typeId);
    }

    @Override
    public Map<String, List<String>> getAllKeywords() {
        return primary.getAllKeywords();
    }

    // ========== 模式管理 ==========

    @Override
    public boolean savePatterns(String typeId, List<String> patterns) {
        boolean result = primary.savePatterns(typeId, patterns);

        asyncWriteToSecondaries(
            () -> secondaries.forEach(p -> p.savePatterns(typeId, patterns)),
            "savePatterns"
        );

        return result;
    }

    @Override
    public boolean addPatterns(String typeId, List<String> patterns) {
        boolean result = primary.addPatterns(typeId, patterns);

        asyncWriteToSecondaries(
            () -> secondaries.forEach(p -> p.addPatterns(typeId, patterns)),
            "addPatterns"
        );

        return result;
    }

    @Override
    public List<String> getPatterns(String typeId) {
        return primary.getPatterns(typeId);
    }

    @Override
    public Map<String, List<String>> getAllPatterns() {
        return primary.getAllPatterns();
    }

    // ========== 备份与恢复 ==========

    @Override
    public String createBackup() {
        // 备份操作只在主后端执行
        return primary.createBackup();
    }

    @Override
    public boolean restoreFromBackup(String backupId) {
        boolean result = primary.restoreFromBackup(backupId);

        // 恢复后同步到次要后端
        if (result) {
            asyncWriteToSecondaries(
                () -> {
                    List<QuestionTypeConfig> allTypes = primary.getAllQuestionTypes();
                    secondaries.forEach(p -> p.saveQuestionTypes(allTypes));
                },
                "restoreFromBackup"
            );
        }

        return result;
    }

    @Override
    public List<String> listBackups() {
        return primary.listBackups();
    }

    // ========== 版本管理 ==========

    @Override
    public String getVersion() {
        return primary.getVersion();
    }

    @Override
    public boolean saveVersion(String version) {
        boolean result = primary.saveVersion(version);

        asyncWriteToSecondaries(
            () -> secondaries.forEach(p -> p.saveVersion(version)),
            "saveVersion"
        );

        return result;
    }

    // ========== 变更历史 ==========

    @Override
    public List<ChangeRecord> getChangeHistory(int limit) {
        return primary.getChangeHistory(limit);
    }

    @Override
    public boolean recordChange(ChangeRecord change) {
        boolean result = primary.recordChange(change);

        asyncWriteToSecondaries(
            () -> secondaries.forEach(p -> p.recordChange(change)),
            "recordChange"
        );

        return result;
    }


    /**
     * 关闭资源
     */
    public void shutdown() {
        log.info("Shutting down CompositePersistence...");
        executorService.shutdown();
    }
}

