package top.yumbo.ai.omni.marketplace.strategy.adapters.model;

import lombok.AllArgsConstructor;
import lombok.Data;
import top.yumbo.ai.omni.storage.api.model.Chunk;

import java.util.List;

/**
 * 分块输出
 */
@Data
@AllArgsConstructor
public class ChunkingOutput {
    private final List<Chunk> chunks;
}
    