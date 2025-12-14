package top.yumbo.ai.omni.core.ppl;

import lombok.extern.slf4j.Slf4j;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;
import top.yumbo.ai.storage.api.DocumentStorageService;
import top.yumbo.ai.storage.api.model.PPLData;

import java.util.Optional;

/**
 * PPL 存储服务 - 负责 PPL (Prompt Programming Language) 数据的存储和管理
 * (PPL Storage Service - Responsible for PPL data storage and management)
 *
 * <p>
 * 重构说明 (Refactoring Notes):
 * - 使用 DocumentStorageService 接口存储 PPL 数据
 * - 删除硬编码的文件存储
 * - 支持多种存储后端（File/MongoDB/Redis/ES等）
 * </p>
 *
 * @author OmniAgent Team
 * @since 1.0.0 (Refactored)
 * @version 3.0.0 - 重构为可插拔架构
 */
@Slf4j
@Service
public class PPLStorageService {

    private final DocumentStorageService storageService;

    @Autowired
    public PPLStorageService(DocumentStorageService storageService) {
        this.storageService = storageService;
        log.info("PPLStorageService initialized with storage: {}",
                 storageService.getClass().getSimpleName());
    }

    /**
     * 保存 PPL 数据
     */
    public String savePPLData(String documentId, String content, String metadata) {
        if (content == null || content.trim().isEmpty()) {
            log.warn("Empty PPL content for document: {}", documentId);
            return null;
        }

        try {
            // PPLData 用于存储分析结果
            PPLData pplData = PPLData.builder()
                .documentId(documentId)
                .analyzedAt(System.currentTimeMillis())
                .build();

            String pplId = storageService.savePPLData(documentId, pplData);
            log.info("Saved PPL data for document {}: {} chars", documentId, content.length());
            return pplId;
        } catch (Exception e) {
            log.error("Failed to save PPL data for document: {}", documentId, e);
            return null;
        }
    }

    /**
     * 获取 PPL 数据
     */
    public Optional<PPLData> getPPLData(String documentId) {
        try {
            return storageService.getPPLData(documentId);
        } catch (Exception e) {
            log.error("Failed to get PPL data for document: {}", documentId, e);
            return Optional.empty();
        }
    }

    /**
     * 删除 PPL 数据
     */
    public void deletePPLData(String documentId) {
        try {
            storageService.deletePPLData(documentId);
            log.info("Deleted PPL data for document: {}", documentId);
        } catch (Exception e) {
            log.error("Failed to delete PPL data for document: {}", documentId, e);
        }
    }
}

