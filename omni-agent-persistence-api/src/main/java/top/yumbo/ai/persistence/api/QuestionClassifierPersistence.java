package top.yumbo.ai.persistence.api;

import top.yumbo.ai.persistence.api.model.QuestionTypeConfig;

import java.util.List;
import java.util.Map;
import java.util.Optional;

/**
 * 问题分类器持久化接口
 * (Question Classifier Persistence Interface)
 *
 * <h3>职责范围 (Responsibilities)</h3>
 * <p>本接口用于持久化<strong>系统配置和元数据</strong>，专门管理问题分类器的配置信息</p>
 *
 * <h3>适用场景 (Use Cases)</h3>
 * <ul>
 *   <li>✅ 存储问题类型配置（结构化小数据）</li>
 *   <li>✅ 管理关键词和模式规则</li>
 *   <li>✅ 版本管理和变更历史</li>
 *   <li>✅ 数据量小（KB级别），访问频繁</li>
 * </ul>
 *
 * <h3>不适用场景 (Not For)</h3>
 * <ul>
 *   <li>❌ 大文件存储（请使用 {@link top.yumbo.ai.storage.api.DocumentStorageService}）</li>
 *   <li>❌ 二进制内容（请使用 DocumentStorageService）</li>
 *   <li>❌ 用户业务数据（请使用 DocumentStorageService）</li>
 * </ul>
 *
 * <h3>支持的后端 (Supported Backends)</h3>
 * <p>Memory, H2, SQLite, Redis, MongoDB, Elasticsearch</p>
 *
 * <h3>与 Storage 层的区别 (vs Storage Layer)</h3>
 * <table border="1">
 *   <tr><th>特性</th><th>Persistence (本接口)</th><th>Storage</th></tr>
 *   <tr><td>数据类型</td><td>结构化配置</td><td>非结构化内容</td></tr>
 *   <tr><td>数据量</td><td>小（KB）</td><td>大（MB-GB）</td></tr>
 *   <tr><td>用途</td><td>系统配置</td><td>业务数据</td></tr>
 *   <tr><td>类比</td><td>图书馆"目录"</td><td>图书馆"书架"</td></tr>
 * </table>
 *
 * @author OmniAgent Team
 * @since 1.0.0
 * @see top.yumbo.ai.storage.api.DocumentStorageService 文档和内容存储服务
 */
public interface QuestionClassifierPersistence {

    // ========== 问题类型管理 (Question Type Management) ==========

    /**
     * 保存问题类型
     * @param config 问题类型配置
     * @return 是否成功
     */
    boolean saveQuestionType(QuestionTypeConfig config);

    /**
     * 批量保存问题类型
     * @param configs 问题类型配置列表
     * @return 成功保存的数量
     */
    int saveQuestionTypes(List<QuestionTypeConfig> configs);

    /**
     * 获取问题类型
     * @param typeId 类型ID
     * @return 问题类型配置
     */
    Optional<QuestionTypeConfig> getQuestionType(String typeId);

    /**
     * 获取所有问题类型
     * @return 所有问题类型配置
     */
    List<QuestionTypeConfig> getAllQuestionTypes();

    /**
     * 更新问题类型
     * @param config 问题类型配置
     * @return 是否成功
     */
    boolean updateQuestionType(QuestionTypeConfig config);

    /**
     * 删除问题类型
     * @param typeId 类型ID
     * @return 是否成功
     */
    boolean deleteQuestionType(String typeId);

    // ========== 关键词管理 (Keywords Management) ==========

    /**
     * 保存类型关键词
     * @param typeId 类型ID
     * @param keywords 关键词列表
     * @return 是否成功
     */
    boolean saveKeywords(String typeId, List<String> keywords);

    /**
     * 添加关键词
     * @param typeId 类型ID
     * @param keywords 要添加的关键词
     * @return 是否成功
     */
    boolean addKeywords(String typeId, List<String> keywords);

    /**
     * 获取类型关键词
     * @param typeId 类型ID
     * @return 关键词列表
     */
    List<String> getKeywords(String typeId);

    /**
     * 获取所有关键词映射
     * @return typeId -> keywords 的映射
     */
    Map<String, List<String>> getAllKeywords();

    // ========== 模式管理 (Patterns Management) ==========

    /**
     * 保存类型模式
     * @param typeId 类型ID
     * @param patterns 正则表达式模式列表
     * @return 是否成功
     */
    boolean savePatterns(String typeId, List<String> patterns);

    /**
     * 添加模式
     * @param typeId 类型ID
     * @param patterns 要添加的模式
     * @return 是否成功
     */
    boolean addPatterns(String typeId, List<String> patterns);

    /**
     * 获取类型模式
     * @param typeId 类型ID
     * @return 模式列表
     */
    List<String> getPatterns(String typeId);

    /**
     * 获取所有模式映射
     * @return typeId -> patterns 的映射
     */
    Map<String, List<String>> getAllPatterns();

    // ========== 备份与恢复 (Backup & Restore) ==========

    /**
     * 创建备份
     * @return 备份ID
     */
    String createBackup();

    /**
     * 从备份恢复
     * @param backupId 备份ID
     * @return 是否成功
     */
    boolean restoreFromBackup(String backupId);

    /**
     * 列出所有备份
     * @return 备份ID列表
     */
    List<String> listBackups();

    // ========== 版本管理 (Version Management) ==========

    /**
     * 获取版本号
     * @return 版本号
     */
    String getVersion();

    /**
     * 保存版本号
     * @param version 版本号
     * @return 是否成功
     */
    boolean saveVersion(String version);

    // ========== 变更历史 (Change History) ==========

    /**
     * 获取变更历史
     * @param limit 限制数量
     * @return 变更记录列表
     */
    List<ChangeRecord> getChangeHistory(int limit);

    /**
     * 记录变更
     * @param change 变更记录
     * @return 是否成功
     */
    boolean recordChange(ChangeRecord change);

    /**
     * 变更记录
     */
    interface ChangeRecord {
        String getId();
        String getTypeId();
        String getAction(); // CREATE, UPDATE, DELETE
        String getOperator();
        long getTimestamp();
        Map<String, Object> getDetails();
    }
}

