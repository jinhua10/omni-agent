package top.yumbo.ai.omni.knowledge.registry.jackson;

import com.fasterxml.jackson.core.JsonParser;
import com.fasterxml.jackson.databind.DeserializationContext;
import com.fasterxml.jackson.databind.JsonDeserializer;
import com.fasterxml.jackson.databind.JsonNode;
import top.yumbo.ai.omni.knowledge.registry.model.DomainType;

import java.io.IOException;
import java.util.HashMap;
import java.util.Map;

/**
 * DomainType JSON 反序列化器
 *
 * <p>处理两种情况：</p>
 * <ul>
 *     <li>字符串形式：直接作为code查找或创建</li>
 *     <li>对象形式：包含完整的code、name、description等字段</li>
 * </ul>
 *
 * @author OmniAgent Team
 * @since 2.0.0
 */
public class DomainTypeDeserializer extends JsonDeserializer<DomainType> {

    @Override
    public DomainType deserialize(JsonParser p, DeserializationContext ctxt) throws IOException {
        JsonNode node = p.getCodec().readTree(p);

        // 情况1：字符串形式 "DOCUMENT"
        if (node.isTextual()) {
            String code = node.asText();
            DomainType type = DomainType.of(code);
            if (type != null) {
                return type;
            }
            // 如果不存在，创建一个最小化的类型
            return DomainType.getOrCreate(code, code);
        }

        // 情况2：对象形式 {"code": "DOCUMENT", "name": "文档知识域", ...}
        if (node.isObject()) {
            String code = node.has("code") ? node.get("code").asText() : null;
            if (code == null) {
                throw new IOException("DomainType must have a 'code' field");
            }

            // 尝试从注册表获取
            DomainType existing = DomainType.of(code);
            if (existing != null) {
                // 如果已存在，直接返回（保证单例）
                return existing;
            }

            // 如果不存在，创建新的并注册
            String name = node.has("name") ? node.get("name").asText() : code;
            String description = node.has("description") ? node.get("description").asText() : null;
            String icon = node.has("icon") ? node.get("icon").asText() : null;

            // 处理metadata
            Map<String, Object> metadata = new HashMap<>();
            if (node.has("metadata") && node.get("metadata").isObject()) {
                JsonNode metadataNode = node.get("metadata");
                metadataNode.fields().forEachRemaining(entry -> {
                    metadata.put(entry.getKey(), entry.getValue().asText());
                });
            }

            DomainType newType = DomainType.builder()
                    .code(code)
                    .name(name)
                    .description(description)
                    .icon(icon)
                    .metadata(metadata)
                    .build();

            return DomainType.register(newType);
        }

        throw new IOException("Invalid DomainType format: " + node);
    }
}

