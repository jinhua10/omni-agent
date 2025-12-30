package top.yumbo.ai.omni.knowledge.registry.model.domain;

import com.fasterxml.jackson.annotation.JsonCreator;
import com.fasterxml.jackson.annotation.JsonProperty;
import com.fasterxml.jackson.databind.annotation.JsonDeserialize;
import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;
import top.yumbo.ai.omni.knowledge.registry.jackson.DomainTypeDeserializer;

import java.io.Serial;
import java.io.Serializable;
import java.util.HashMap;
import java.util.Map;
import java.util.Objects;
import java.util.concurrent.ConcurrentHashMap;

/**
 * 知识域类型（支持动态注册）
 *
 * <p>从枚举重构为类，支持用户自定义知识域类型</p>
 *
 * <p>设计理念：</p>
 * <ul>
 *     <li>预定义常用类型（常量方式）</li>
 *     <li>支持动态注册自定义类型</li>
 *     <li>全局类型注册表</li>
 *     <li>类型校验与去重</li>
 * </ul>
 *
 * @author OmniAgent Team
 * @since 2.0.0
 */
@Data
@Builder
@NoArgsConstructor
@AllArgsConstructor
@JsonDeserialize(using = DomainTypeDeserializer.class)
public class DomainType implements Serializable {

    @Serial
    private static final long serialVersionUID = 1L;

    /**
     * 全局类型注册表
     */
    private static final Map<String, DomainType> REGISTRY = new ConcurrentHashMap<>();

    /**
     * 类型代码（唯一标识）
     */
    @JsonProperty("code")
    private String code;

    /**
     * 类型名称
     */
    private String name;

    /**
     * 描述
     */
    private String description;

    /**
     * 图标
     */
    private String icon;

    /**
     * 扩展属性
     */
    @Builder.Default
    private Map<String, Object> metadata = new HashMap<>();

    // ========== 预定义常用类型 ==========

    /**
     * 文档知识域
     */
    public static final DomainType DOCUMENT = register(
        DomainType.builder()
            .code("DOCUMENT")
            .name("文档知识域")
            .description("用于存储和管理各类文档知识，如技术文档、教程、手册等")
            .icon("📄")
            .build()
    );

    /**
     * 源码知识域
     */
    public static final DomainType SOURCE_CODE = register(
        DomainType.builder()
            .code("SOURCE_CODE")
            .name("源码知识域")
            .description("用于存储和管理项目源代码、代码片段、技术实现等")
            .icon("💻")
            .build()
    );

    /**
     * 角色知识域
     */
    public static final DomainType ROLE_KNOWLEDGE = register(
        DomainType.builder()
            .code("ROLE_KNOWLEDGE")
            .name("角色知识域")
            .description("用于存储和管理特定角色的专业知识，如架构师、测试工程师等")
            .icon("👤")
            .build()
    );

    /**
     * API文档域
     */
    public static final DomainType API_DOCUMENTATION = register(
        DomainType.builder()
            .code("API_DOCUMENTATION")
            .name("API文档域")
            .description("用于存储和管理API接口文档、OpenAPI规范等")
            .icon("🔌")
            .build()
    );

    /**
     * 混合域
     */
    public static final DomainType MIXED = register(
        DomainType.builder()
            .code("MIXED")
            .name("混合域")
            .description("包含多种类型知识的综合域")
            .icon("🎯")
            .build()
    );

    /**
     * 业务知识域
     */
    public static final DomainType BUSINESS = register(
        DomainType.builder()
            .code("BUSINESS")
            .name("业务知识域")
            .description("用于存储业务规则、流程、需求等业务相关知识")
            .icon("💼")
            .build()
    );

    /**
     * 测试知识域
     */
    public static final DomainType TEST = register(
        DomainType.builder()
            .code("TEST")
            .name("测试知识域")
            .description("用于存储测试用例、测试策略、质量标准等")
            .icon("🧪")
            .build()
    );

    /**
     * 运维知识域
     */
    public static final DomainType OPERATIONS = register(
        DomainType.builder()
            .code("OPERATIONS")
            .name("运维知识域")
            .description("用于存储部署、监控、运维脚本等运维相关知识")
            .icon("⚙️")
            .build()
    );

    // ========== 动态注册方法 ==========

    /**
     * 注册新的知识域类型
     *
     * @param domainType 域类型
     * @return 注册后的域类型
     */
    public static DomainType register(DomainType domainType) {
        if (domainType == null || domainType.getCode() == null) {
            throw new IllegalArgumentException("DomainType and code cannot be null");
        }

        String code = domainType.getCode().toUpperCase();
        domainType.setCode(code);

        // 如果已存在，返回已存在的（保证单例）
        return REGISTRY.computeIfAbsent(code, k -> domainType);
    }

    /**
     * 根据代码获取域类型
     * 
     * <p>此方法也用于JSON反序列化和Spring参数转换</p>
     *
     * @param code 类型代码
     * @return 域类型，如果不存在返回null
     */
    @JsonCreator
    public static DomainType of(@JsonProperty("code") String code) {
        if (code == null) {
            return null;
        }
        return REGISTRY.get(code.toUpperCase());
    }

    /**
     * 根据代码获取域类型，如果不存在则创建
     *
     * @param code 类型代码
     * @param name 类型名称
     * @return 域类型
     */
    public static DomainType getOrCreate(String code, String name) {
        DomainType existing = of(code);
        if (existing != null) {
            return existing;
        }

        return register(
            DomainType.builder()
                .code(code)
                .name(name != null ? name : code)
                .description("自定义知识域类型")
                .icon("📦")
                .build()
        );
    }

    /**
     * 检查类型是否已注册
     *
     * @param code 类型代码
     * @return 是否已注册
     */
    public static boolean isRegistered(String code) {
        return code != null && REGISTRY.containsKey(code.toUpperCase());
    }

    /**
     * 获取所有已注册的类型
     *
     * @return 所有类型的副本
     */
    public static Map<String, DomainType> getAllTypes() {
        return new HashMap<>(REGISTRY);
    }

    /**
     * 取消注册（谨慎使用）
     *
     * @param code 类型代码
     * @return 被移除的类型
     */
    public static DomainType unregister(String code) {
        if (code == null) {
            return null;
        }
        return REGISTRY.remove(code.toUpperCase());
    }

    /**
     * 清空所有自定义类型（保留预定义类型）
     */
    public static void clearCustomTypes() {
        REGISTRY.keySet().removeIf(code ->
            !code.equals("DOCUMENT") &&
            !code.equals("SOURCE_CODE") &&
            !code.equals("ROLE_KNOWLEDGE") &&
            !code.equals("API_DOCUMENTATION") &&
            !code.equals("MIXED") &&
            !code.equals("BUSINESS") &&
            !code.equals("TEST") &&
            !code.equals("OPERATIONS")
        );
    }

    // ========== Object方法 ==========

    @Override
    public boolean equals(Object o) {
        if (this == o) return true;
        if (o == null || getClass() != o.getClass()) return false;
        DomainType that = (DomainType) o;
        return Objects.equals(code, that.code);
    }

    @Override
    public int hashCode() {
        return Objects.hash(code);
    }

    @Override
    public String toString() {
        return String.format("DomainType{code='%s', name='%s'}", code, name);
    }

    /**
     * 获取显示名称
     *
     * @return 名称或代码
     */
    public String getDisplayName() {
        return name != null ? name : code;
    }
}

