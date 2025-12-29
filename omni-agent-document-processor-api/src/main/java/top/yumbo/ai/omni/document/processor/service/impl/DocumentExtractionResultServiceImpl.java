package top.yumbo.ai.omni.document.processor.service.impl;

import com.fasterxml.jackson.databind.ObjectMapper;
import lombok.RequiredArgsConstructor;
import lombok.extern.slf4j.Slf4j;
import top.yumbo.ai.omni.document.processor.model.DocumentExtractionResult;
import top.yumbo.ai.omni.document.processor.service.DocumentExtractionResultService;
import top.yumbo.ai.omni.storage.api.DocumentStorageService;

import java.util.*;
import java.util.stream.Collectors;

/**
 * 文档提取结果管理服务实现
 * (Document Extraction Result Management Service Implementation)
 *
 * <p>使用 DocumentStorageService 实现持久化，支持多种存储后端</p>
 * <p>支持的存储方式：File/MongoDB/Redis/S3/MinIO/Elasticsearch</p>
 *
 * @author OmniAgent Team
 * @since 3.0.0
 */
@Slf4j
@RequiredArgsConstructor
public class DocumentExtractionResultServiceImpl implements DocumentExtractionResultService {

    private final DocumentStorageService storageService;
    private final ObjectMapper objectMapper = new ObjectMapper();

    /**
     * 虚拟目录前缀（用于隔离提取结果）
     */
    private static final String STORAGE_PREFIX = "extracted/";

    /**
     * 索引文档ID（用于存储所有提取结果的ID列表）
     */
    private static final String INDEX_DOC_ID = "extracted/_index";

    /**
     * 获取存储路径
     */
    private String getStoragePath(String documentId) {
        return STORAGE_PREFIX + sanitizeDocumentId(documentId) + ".json";
    }

    /**
     * 清理文档ID，避免路径安全问题
     */
    private String sanitizeDocumentId(String documentId) {
        // 替换路径分隔符和特殊字符
        return documentId.replaceAll("[/\\\\]", "_");
    }

    /**
     * 添加到索引
     */
    private synchronized void addToIndex(String documentId) {
        try {
            Set<String> index = loadIndex();
            if (index.add(documentId)) {
                saveIndex(index);
            }
        } catch (Exception e) {
            log.warn("添加到索引失败: {}", documentId, e);
        }
    }

    /**
     * 从索引移除
     */
    private synchronized void removeFromIndex(String documentId) {
        try {
            Set<String> index = loadIndex();
            if (index.remove(documentId)) {
                saveIndex(index);
            }
        } catch (Exception e) {
            log.warn("从索引移除失败: {}", documentId, e);
        }
    }

    /**
     * 加载索引
     */
    private Set<String> loadIndex() {
        try {
            Optional<byte[]> indexData = storageService.getDocument(INDEX_DOC_ID);
            if (indexData.isEmpty()) {
                return new HashSet<>();
            }

            String jsonContent = new String(indexData.get(), java.nio.charset.StandardCharsets.UTF_8);
            String[] ids = objectMapper.readValue(jsonContent, String[].class);
            return new HashSet<>(Arrays.asList(ids));
        } catch (Exception e) {
            log.warn("加载索引失败", e);
            return new HashSet<>();
        }
    }

    /**
     * 保存索引
     */
    private void saveIndex(Set<String> index) {
        try {
            String jsonContent = objectMapper.writeValueAsString(index.toArray(new String[0]));
            byte[] content = jsonContent.getBytes(java.nio.charset.StandardCharsets.UTF_8);
            // ⭐ 使用 INDEX_DOC_ID 作为 documentId，"extracted/_index.json" 作为 filename
            storageService.saveDocument(INDEX_DOC_ID, "extracted/_index.json", content);
        } catch (Exception e) {
            log.error("保存索引失败", e);
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

            // 序列化为JSON
            String jsonContent = objectMapper.writerWithDefaultPrettyPrinter()
                    .writeValueAsString(result);
            byte[] content = jsonContent.getBytes(java.nio.charset.StandardCharsets.UTF_8);

            // 保存到虚拟存储（支持多种后端）
            // ⭐ 使用 extracted/ 前缀作为 filename，让 FileDocumentStorage 路由到 extracted/ 目录
            String fileName = getStoragePath(result.getDocumentId());  // "extracted/xxx.json"
            String documentId = result.getDocumentId();  // 文档ID用于标识
            storageService.saveDocument(documentId, fileName, content);

            // 添加到索引
            addToIndex(result.getDocumentId());

            log.info("💾 文档提取结果已保存: documentId={}, status={}, textLength={}, storage={}",
                    result.getDocumentId(), result.getStatus(),
                    result.getExtractedText() != null ? result.getExtractedText().length() : 0,
                    storageService.getClass().getSimpleName());

            return result;

        } catch (Exception e) {
            log.error("❌ 保存文档提取结果失败: documentId={}", result.getDocumentId(), e);
            throw new RuntimeException("保存文档提取结果失败", e);
        }
    }

    @Override
    public Optional<DocumentExtractionResult> findByDocumentId(String documentId) {
        try {
            // ⭐ 使用 extracted/ 前缀作为路径，让 FileDocumentStorage 从 extracted/ 目录读取
            String storagePath = getStoragePath(documentId);  // "extracted/xxx.json"

            // 从虚拟存储读取
            Optional<byte[]> contentOpt = storageService.getDocument(storagePath);
            if (contentOpt.isEmpty()) {
                log.debug("未找到文档提取结果: documentId={}, path={}", documentId, storagePath);
                return Optional.empty();
            }

            // 反序列化JSON
            String jsonContent = new String(contentOpt.get(), java.nio.charset.StandardCharsets.UTF_8);
            DocumentExtractionResult result = objectMapper.readValue(
                    jsonContent,
                    DocumentExtractionResult.class
            );

            log.debug("成功读取文档提取结果: documentId={}, textLength={}",
                    documentId, result.getExtractedText() != null ? result.getExtractedText().length() : 0);
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
            String storagePath = getStoragePath(documentId);
            storageService.deleteDocument(storagePath);

            // 从索引移除
            removeFromIndex(documentId);

            log.info("🗑️ 文档提取结果已删除: documentId={}", documentId);

        } catch (Exception e) {
            log.error("❌ 删除文档提取结果失败: documentId={}", documentId, e);
            throw new RuntimeException("删除文档提取结果失败", e);
        }
    }

    @Override
    public List<DocumentExtractionResult> findAll() {
        try {
            // 从索引加载所有文档ID
            Set<String> documentIds = loadIndex();

            return documentIds.stream()
                    .map(this::findByDocumentId)
                    .filter(Optional::isPresent)
                    .map(Optional::get)
                    .collect(Collectors.toList());

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
}


