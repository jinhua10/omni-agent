package top.yumbo.ai.omni.marketplace.strategy.adapters.model;

import lombok.AllArgsConstructor;
import lombok.Data;

/**
 * 分块输入
 */
@Data
@AllArgsConstructor
public class ChunkingInput {
    private final String documentId;
    private final String content;
}

