package top.yumbo.ai.omni.core.document.service.impl;

import com.fasterxml.jackson.databind.ObjectMapper;
import lombok.extern.slf4j.Slf4j;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.stereotype.Service;
import top.yumbo.ai.omni.core.document.model.DocumentExtractionResult;
import top.yumbo.ai.omni.core.document.service.DocumentExtractionResultService;

import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.Paths;
import java.util.*;
import java.util.stream.Collectors;
import java.util.stream.Stream;

/**
 * 文档提取结果管理服务实现
 * (Document Extraction Result Management Service Implementation)
 *
 * <p>使用基于文件的JSON存储实现持久化</p>
 *
 * @author OmniAgent Team
 * @since 3.0.0
 */
@Slf4j
@Service
public class DocumentExtractionResultServiceImpl implements DocumentExtractionResultService {

    private final ObjectMapper objectMapper = new ObjectMapper();
    private final Path storageDir;

    public DocumentExtractionResultServiceImpl(
            @Value("${omni-agent.data-dir:./data}") String dataDir) {
        this.storageDir = Paths.get(dataDir, "extraction-results");
        try {
            Files.createDirectories(storageDir);
            log.info("📁 文档提取结果存储目录: {}", storageDir.toAbsolutePath());
        } catch (IOException e) {
            log.error("创建存储目录失败", e);
            throw new RuntimeException("初始化文档提取结果服务失败", e);
        }
    }

    @Override
    public DocumentExtractionResult save(DocumentExtractionResult result) {
        try {
            // 设置时间戳
            long now = System.currentTimeMillis();
            if (result.getCreatedAt() == null) {
                result.setCreatedAt(now);
            }
            result.setUpdatedAt(now);

            // 增加版本号
            if (result.getVersion() == null) {
                result.setVersion(1);
            } else {
                result.setVersion(result.getVersion() + 1);
            }

            // 保存为JSON文件
            Path filePath = getFilePath(result.getDocumentId());
            objectMapper.writerWithDefaultPrettyPrinter().writeValue(filePath.toFile(), result);

            log.info("💾 文档提取结果已保存: documentId={}, status={}, textLength={}",
                    result.getDocumentId(), result.getStatus(),
                    result.getExtractedText() != null ? result.getExtractedText().length() : 0);

            return result;

        } catch (Exception e) {
            log.error("❌ 保存文档提取结果失败: documentId={}", result.getDocumentId(), e);
            throw new RuntimeException("保存文档提取结果失败", e);
        }
    }

    @Override
    public Optional<DocumentExtractionResult> findByDocumentId(String documentId) {
        try {
            Path filePath = getFilePath(documentId);
            if (!Files.exists(filePath)) {
                return Optional.empty();
            }

            DocumentExtractionResult result = objectMapper.readValue(
                    filePath.toFile(),
                    DocumentExtractionResult.class
            );
            return Optional.of(result);

        } catch (Exception e) {
            log.error("❌ 获取文档提取结果失败: documentId={}", documentId, e);
            return Optional.empty();
        }
    }

    @Override
    public boolean isExtracted(String documentId) {
        Optional<DocumentExtractionResult> result = findByDocumentId(documentId);
        return result.map(r -> "COMPLETED".equals(r.getStatus()) &&
                r.getExtractedText() != null &&
                !r.getExtractedText().isEmpty()
        ).orElse(false);
    }

    @Override
    public boolean needsReExtraction(String documentId, String currentMd5) {
        Optional<DocumentExtractionResult> result = findByDocumentId(documentId);

        if (result.isEmpty()) {
            // 没有提取记录，需要提取
            return true;
        }

        DocumentExtractionResult existing = result.get();

        // 检查状态
        if ("FAILED".equals(existing.getStatus())) {
            log.debug("📝 文档需要重新提取（上次失败）: {}", documentId);
            return true;
        }

        // 检查MD5是否变化
        if (currentMd5 != null && !currentMd5.equals(existing.getFileMd5())) {
            log.debug("📝 文档需要重新提取（文件已变化）: {}", documentId);
            return true;
        }

        // 检查内容是否为空
        if (existing.getExtractedText() == null || existing.getExtractedText().isEmpty()) {
            log.debug("📝 文档需要重新提取（内容为空）: {}", documentId);
            return true;
        }

        log.debug("✅ 文档无需重新提取: {}", documentId);
        return false;
    }

    @Override
    public void delete(String documentId) {
        try {
            Path filePath = getFilePath(documentId);
            Files.deleteIfExists(filePath);
            log.info("🗑️ 文档提取结果已删除: documentId={}", documentId);

        } catch (Exception e) {
            log.error("❌ 删除文档提取结果失败: documentId={}", documentId, e);
            throw new RuntimeException("删除文档提取结果失败", e);
        }
    }

    @Override
    public List<DocumentExtractionResult> findAll() {
        try {
            if (!Files.exists(storageDir)) {
                return Collections.emptyList();
            }

            try (Stream<Path> paths = Files.list(storageDir)) {
                return paths
                        .filter(path -> path.toString().endsWith(".json"))
                        .map(this::loadFromFile)
                        .filter(Objects::nonNull)
                        .collect(Collectors.toList());
            }

        } catch (Exception e) {
            log.error("❌ 获取所有文档提取结果失败", e);
            return Collections.emptyList();
        }
    }

    @Override
    public List<DocumentExtractionResult> findByStatus(String status) {
        try {
            return findAll().stream()
                    .filter(r -> status.equals(r.getStatus()))
                    .collect(Collectors.toList());

        } catch (Exception e) {
            log.error("❌ 按状态查询文档提取结果失败: status={}", status, e);
            return Collections.emptyList();
        }
    }

    @Override
    public int cleanupFailedRecords(long olderThan) {
        try {
            List<DocumentExtractionResult> failedRecords = findByStatus("FAILED");
            int cleaned = 0;

            for (DocumentExtractionResult record : failedRecords) {
                if (record.getCompletedTime() != null && record.getCompletedTime() < olderThan) {
                    delete(record.getDocumentId());
                    cleaned++;
                }
            }

            log.info("🧹 清理失败的提取记录: 共清理 {} 条", cleaned);
            return cleaned;

        } catch (Exception e) {
            log.error("❌ 清理失败记录失败", e);
            return 0;
        }
    }

    @Override
    public Map<String, Object> getStatistics() {
        try {
            List<DocumentExtractionResult> all = findAll();

            long completed = all.stream().filter(r -> "COMPLETED".equals(r.getStatus())).count();
            long failed = all.stream().filter(r -> "FAILED".equals(r.getStatus())).count();
            long pending = all.stream().filter(r -> "PENDING".equals(r.getStatus())).count();
            long extracting = all.stream().filter(r -> "EXTRACTING".equals(r.getStatus())).count();

            long totalChars = all.stream()
                    .filter(r -> r.getExtractedText() != null)
                    .mapToLong(r -> r.getExtractedText().length())
                    .sum();

            OptionalDouble avgDuration = all.stream()
                    .filter(r -> r.getDuration() != null && r.getDuration() > 0)
                    .mapToLong(DocumentExtractionResult::getDuration)
                    .average();

            Map<String, Object> stats = new HashMap<>();
            stats.put("total", all.size());
            stats.put("completed", completed);
            stats.put("failed", failed);
            stats.put("pending", pending);
            stats.put("extracting", extracting);
            stats.put("totalCharacters", totalChars);
            stats.put("averageDuration", avgDuration.orElse(0.0));

            return stats;

        } catch (Exception e) {
            log.error("❌ 获取统计信息失败", e);
            return Collections.emptyMap();
        }
    }

    // ========== 辅助方法 ==========

    /**
     * 获取文件路径
     */
    private Path getFilePath(String documentId) {
        // 对文档ID进行编码，避免文件名非法字符
        String safeFileName = documentId.replaceAll("[^a-zA-Z0-9._-]", "_") + ".json";
        return storageDir.resolve(safeFileName);
    }

    /**
     * 从文件加载
     */
    private DocumentExtractionResult loadFromFile(Path filePath) {
        try {
            return objectMapper.readValue(filePath.toFile(), DocumentExtractionResult.class);
        } catch (Exception e) {
            log.warn("加载文件失败: {}", filePath, e);
            return null;
        }
    }
}

