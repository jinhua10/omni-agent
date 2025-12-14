package top.yumbo.ai.p2p.api;

import top.yumbo.ai.p2p.api.model.ConnectionCode;
import top.yumbo.ai.p2p.api.model.PeerConnection;
import top.yumbo.ai.p2p.api.model.SharedKnowledge;

import java.util.List;
import java.util.Optional;

/**
 * P2P协作服务接口
 * (P2P Collaboration Service Interface)
 *
 * <p>所有P2P实现必须实现此接口</p>
 * <p>支持点对点知识共享和协作</p>
 *
 * @author OmniAgent Team
 * @since 1.0.0
 */
public interface P2PCollaborationService {

    // ========== 连接管理 (Connection Management) ==========

    /**
     * 生成连接码
     * (Generate connection code)
     *
     * @param userId 用户ID
     * @param userName 用户昵称
     * @param validMinutes 有效时长（分钟）
     * @return 连接码
     */
    ConnectionCode generateConnectionCode(String userId, String userName, int validMinutes);

    /**
     * 使用连接码建立连接
     * (Establish connection using code)
     *
     * @param code 连接码
     * @param userId 用户ID
     * @param userName 用户昵称
     * @return P2P连接
     */
    PeerConnection connectWithCode(String code, String userId, String userName);

    /**
     * 断开连接
     * (Disconnect)
     *
     * @param connectionId 连接ID
     * @return 是否成功
     */
    boolean disconnect(String connectionId);

    /**
     * 获取所有连接
     * (Get all connections)
     *
     * @param userId 用户ID
     * @return 连接列表
     */
    List<PeerConnection> getConnections(String userId);

    /**
     * 获取连接详情
     * (Get connection details)
     *
     * @param connectionId 连接ID
     * @return 连接信息
     */
    Optional<PeerConnection> getConnection(String connectionId);

    // ========== 知识共享 (Knowledge Sharing) ==========

    /**
     * 分享知识给同事
     * (Share knowledge to peer)
     *
     * @param connectionId 连接ID
     * @param knowledge 知识内容
     * @return 共享知识
     */
    SharedKnowledge shareKnowledge(String connectionId, SharedKnowledge knowledge);

    /**
     * 接收共享的知识
     * (Receive shared knowledge)
     *
     * @param connectionId 连接ID
     * @return 知识列表
     */
    List<SharedKnowledge> receiveKnowledge(String connectionId);

    /**
     * 验证知识质量
     * (Verify knowledge quality)
     *
     * @param knowledgeId 知识ID
     * @param qualityScore 质量评分
     * @return 是否成功
     */
    boolean verifyKnowledge(String knowledgeId, double qualityScore);

    /**
     * 获取共享统计
     * (Get sharing statistics)
     *
     * @param userId 用户ID
     * @return 统计信息
     */
    java.util.Map<String, Object> getSharingStatistics(String userId);

    // ========== 加密服务 (Encryption Service) ==========

    /**
     * 加密内容
     * (Encrypt content)
     *
     * @param content 原始内容
     * @param connectionId 连接ID
     * @return 加密后的内容
     */
    String encrypt(String content, String connectionId);

    /**
     * 解密内容
     * (Decrypt content)
     *
     * @param encryptedContent 加密内容
     * @param connectionId 连接ID
     * @return 原始内容
     */
    String decrypt(String encryptedContent, String connectionId);
}

