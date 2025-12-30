package top.yumbo.ai.omni.example.basic.domain;

import lombok.extern.slf4j.Slf4j;
import org.springframework.stereotype.Component;
import top.yumbo.ai.omni.knowledge.registry.model.DomainType;
import top.yumbo.ai.omni.knowledge.registry.model.KnowledgeDomain;

import jakarta.annotation.PostConstruct;
import java.util.Map;

/**
 * DomainType 使用示例
 *
 * @author OmniAgent Team
 * @since 2.0.0
 */
@Slf4j
@Component
public class DomainTypeExample {

    @PostConstruct
    public void init() {
        log.info("\n=== DomainType 使用示例 ===\n");

        // 示例1：使用预定义类型
        example1_UsePredefinedTypes();

        // 示例2：注册自定义类型
        example2_RegisterCustomType();

        // 示例3：使用 getOrCreate
        example3_GetOrCreate();

        // 示例4：查询所有类型
        example4_GetAllTypes();

        log.info("\n=== 示例完成 ===\n");
    }

    /**
     * 示例1：使用预定义类型
     */
    private void example1_UsePredefinedTypes() {
        log.info("示例1：使用预定义类型");

        // 创建文档域
        KnowledgeDomain docDomain = KnowledgeDomain.builder()
                .domainId("tech-docs-001")
                .domainName("技术文档库")
                .domainType(DomainType.DOCUMENT)
                .description("存储所有技术文档")
                .build();

        log.info("✅ 创建文档域: {} (类型: {})",
                docDomain.getDomainName(),
                docDomain.getDomainType().getName());

        // 创建源码域
        KnowledgeDomain codeDomain = KnowledgeDomain.builder()
                .domainId("project-source-001")
                .domainName("项目源码")
                .domainType(DomainType.SOURCE_CODE)
                .description("存储项目源代码")
                .build();

        log.info("✅ 创建源码域: {} (类型: {})",
                codeDomain.getDomainName(),
                codeDomain.getDomainType().getName());

        // 类型比较
        if (DomainType.DOCUMENT.equals(docDomain.getDomainType())) {
            log.info("✅ 类型比较成功：这是一个文档域");
        }
    }

    /**
     * 示例2：注册自定义类型
     */
    private void example2_RegisterCustomType() {
        log.info("\n示例2：注册自定义类型");

        // 注册企业特定的域类型
        DomainType customerDomain = DomainType.register(
                DomainType.builder()
                        .code("CUSTOMER_360")
                        .name("客户360视图")
                        .description("存储客户全方位数据，包括基本信息、交易历史、行为数据等")
                        .icon("👥")
                        .metadata(Map.of(
                                "dataRetention", "7years",
                                "complianceLevel", "high",
                                "encryption", "required"
                        ))
                        .build()
        );

        log.info("✅ 注册自定义类型: {} ({})",
                customerDomain.getName(),
                customerDomain.getCode());

        // 使用自定义类型创建知识域
        KnowledgeDomain domain = KnowledgeDomain.builder()
                .domainId("customer-data-001")
                .domainName("客户数据中心")
                .domainType(customerDomain)
                .build();

        log.info("✅ 使用自定义类型创建域: {}", domain.getDomainName());

        // 获取扩展属性
        String retention = (String) customerDomain.getMetadata().get("dataRetention");
        log.info("✅ 数据保留期: {}", retention);
    }

    /**
     * 示例3：使用 getOrCreate
     */
    private void example3_GetOrCreate() {
        log.info("\n示例3：使用 getOrCreate");

        // 第一次调用 - 创建新类型
        DomainType financialType = DomainType.getOrCreate(
                "FINANCIAL_DATA",
                "财务数据域"
        );
        log.info("✅ 获取/创建财务数据域: {}", financialType.getName());

        // 第二次调用 - 返回已存在的
        DomainType sameType = DomainType.getOrCreate(
                "FINANCIAL_DATA",
                "不同的名称"  // 这个名称会被忽略
        );

        if (financialType.equals(sameType)) {
            log.info("✅ 返回的是同一个实例");
        }
    }

    /**
     * 示例4：查询所有类型
     */
    private void example4_GetAllTypes() {
        log.info("\n示例4：查询所有类型");

        Map<String, DomainType> allTypes = DomainType.getAllTypes();
        log.info("📊 已注册的域类型总数: {}", allTypes.size());

        log.info("\n预定义类型：");
        allTypes.values().stream()
                .filter(type ->
                        type.getCode().equals("DOCUMENT") ||
                        type.getCode().equals("SOURCE_CODE") ||
                        type.getCode().equals("ROLE_KNOWLEDGE") ||
                        type.getCode().equals("API_DOCUMENTATION") ||
                        type.getCode().equals("MIXED") ||
                        type.getCode().equals("BUSINESS") ||
                        type.getCode().equals("TEST") ||
                        type.getCode().equals("OPERATIONS")
                )
                .forEach(type -> log.info("  {} {} - {}",
                        type.getIcon(),
                        type.getCode(),
                        type.getName()));

        log.info("\n自定义类型：");
        allTypes.values().stream()
                .filter(type ->
                        !type.getCode().equals("DOCUMENT") &&
                        !type.getCode().equals("SOURCE_CODE") &&
                        !type.getCode().equals("ROLE_KNOWLEDGE") &&
                        !type.getCode().equals("API_DOCUMENTATION") &&
                        !type.getCode().equals("MIXED") &&
                        !type.getCode().equals("BUSINESS") &&
                        !type.getCode().equals("TEST") &&
                        !type.getCode().equals("OPERATIONS")
                )
                .forEach(type -> log.info("  {} {} - {}",
                        type.getIcon() != null ? type.getIcon() : "📦",
                        type.getCode(),
                        type.getName()));
    }
}

