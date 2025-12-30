package top.yumbo.ai.omni.knowledge.registry.converter;

import org.springframework.core.convert.converter.Converter;
import org.springframework.lang.NonNull;
import org.springframework.stereotype.Component;
import top.yumbo.ai.omni.knowledge.registry.model.DomainType;

/**
 * DomainType 字符串转换器
 *
 * <p>用于支持 Spring MVC 的 @RequestParam 自动转换</p>
 *
 * @author OmniAgent Team
 * @since 2.0.0
 */
@Component
public class DomainTypeConverter implements Converter<String, DomainType> {

    @Override
    public DomainType convert(@NonNull String source) {
        if (source.trim().isEmpty()) {
            return null;
        }

        // 尝试从注册表获取
        DomainType type = DomainType.of(source.trim());

        // 如果不存在，自动创建（可选行为）
        if (type == null) {
            type = DomainType.getOrCreate(source.trim(), source.trim());
        }

        return type;
    }
}

