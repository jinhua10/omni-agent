package top.yumbo.ai.omni.core.document.service;

import top.yumbo.ai.omni.core.document.model.DocumentExtractionResult;

import java.util.List;
import java.util.Optional;

/**
 * 文档提取结果管理服务接口
 * (Document Extraction Result Management Service)
 *
 * <p>负责持久化和管理文档的提取结果</p>
 *
 * @author OmniAgent Team
 * @since 3.0.0
 */
public interface DocumentExtractionResultService {

    /**
     * 保存文档提取结果
     *
     * @param result 提取结果
     * @return 保存后的结果
     */
    DocumentExtractionResult save(DocumentExtractionResult result);

    /**
     * 根据文档ID获取提取结果
     *
     * @param documentId 文档ID
     * @return 提取结果（如果存在）
     */
    Optional<DocumentExtractionResult> findByDocumentId(String documentId);

    /**
     * 检查文档是否已提取
     *
     * @param documentId 文档ID
     * @return true 如果已提取且成功
     */
    boolean isExtracted(String documentId);

    /**
     * 检查文档是否需要重新提取
     * （基于文件MD5或其他条件）
     *
     * @param documentId 文档ID
     * @param currentMd5 当前文件的MD5
     * @return true 如果需要重新提取
     */
    boolean needsReExtraction(String documentId, String currentMd5);

    /**
     * 删除文档提取结果
     *
     * @param documentId 文档ID
     */
    void delete(String documentId);

    /**
     * 获取所有提取结果
     *
     * @return 所有提取结果列表
     */
    List<DocumentExtractionResult> findAll();

    /**
     * 根据状态查询
     *
     * @param status 状态（PENDING, EXTRACTING, COMPLETED, FAILED）
     * @return 符合条件的结果列表
     */
    List<DocumentExtractionResult> findByStatus(String status);

    /**
     * 清理失败的提取记录
     *
     * @param olderThan 清理早于此时间的记录（毫秒时间戳）
     * @return 清理的记录数
     */
    int cleanupFailedRecords(long olderThan);

    /**
     * 获取提取统计信息
     *
     * @return 统计信息Map
     */
    java.util.Map<String, Object> getStatistics();
}


