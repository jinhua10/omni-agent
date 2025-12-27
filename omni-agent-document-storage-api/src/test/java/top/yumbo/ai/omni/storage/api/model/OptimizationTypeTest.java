package top.yumbo.ai.omni.storage.api.model;

import org.junit.jupiter.api.Test;

import static org.junit.jupiter.api.Assertions.*;

/**
 * OptimizationType 枚举类单元测试
 *
 * @author OmniAgent Team
 * @since 2.0.0
 */
class OptimizationTypeTest {

    @Test
    void testAllTypesHaveUniqueCode() {
        // Given
        OptimizationType[] types = OptimizationType.values();

        // When & Then - 确保所有code都是唯一的
        for (int i = 0; i < types.length; i++) {
            for (int j = i + 1; j < types.length; j++) {
                assertNotEquals(types[i].getCode(), types[j].getCode(),
                        "Code should be unique: " + types[i] + " vs " + types[j]);
            }
        }
    }

    @Test
    void testFromCode_ValidCodes() {
        // Test all valid codes
        assertEquals(OptimizationType.PPL, OptimizationType.fromCode("ppl"));
        assertEquals(OptimizationType.HYDE, OptimizationType.fromCode("hyde"));
        assertEquals(OptimizationType.RERANK, OptimizationType.fromCode("rerank"));
        assertEquals(OptimizationType.QUERY_EXPANSION, OptimizationType.fromCode("query_expansion"));
        assertEquals(OptimizationType.QUERY_REWRITE, OptimizationType.fromCode("query_rewrite"));
        assertEquals(OptimizationType.METADATA_FILTER, OptimizationType.fromCode("metadata_filter"));
        assertEquals(OptimizationType.CONTEXT_COMPRESSION, OptimizationType.fromCode("context_compression"));
        assertEquals(OptimizationType.SEMANTIC_CHUNKING, OptimizationType.fromCode("semantic_chunking"));
        assertEquals(OptimizationType.HYBRID_SEARCH, OptimizationType.fromCode("hybrid_search"));
        assertEquals(OptimizationType.KNOWLEDGE_GRAPH, OptimizationType.fromCode("knowledge_graph"));
        assertEquals(OptimizationType.HOPE_ROUTING, OptimizationType.fromCode("hope_routing"));
        assertEquals(OptimizationType.BEHAVIOR_ANALYSIS, OptimizationType.fromCode("behavior_analysis"));
        assertEquals(OptimizationType.MULTI_MODEL_VOTING, OptimizationType.fromCode("multi_model_voting"));
        assertEquals(OptimizationType.CUSTOM, OptimizationType.fromCode("custom"));
    }

    @Test
    void testFromCode_CaseInsensitive() {
        // Test case insensitivity
        assertEquals(OptimizationType.PPL, OptimizationType.fromCode("PPL"));
        assertEquals(OptimizationType.PPL, OptimizationType.fromCode("Ppl"));
        assertEquals(OptimizationType.HYDE, OptimizationType.fromCode("HYDE"));
        assertEquals(OptimizationType.RERANK, OptimizationType.fromCode("RERANK"));
    }

    @Test
    void testFromCode_InvalidCode() {
        // Invalid codes should return CUSTOM
        assertEquals(OptimizationType.CUSTOM, OptimizationType.fromCode("invalid_code"));
        assertEquals(OptimizationType.CUSTOM, OptimizationType.fromCode("unknown"));
        assertEquals(OptimizationType.CUSTOM, OptimizationType.fromCode(""));
    }

    @Test
    void testIsValid() {
        // Valid codes
        assertTrue(OptimizationType.isValid("ppl"));
        assertTrue(OptimizationType.isValid("hyde"));
        assertTrue(OptimizationType.isValid("rerank"));
        assertTrue(OptimizationType.isValid("query_expansion"));
        assertTrue(OptimizationType.isValid("custom"));

        // Case insensitive
        assertTrue(OptimizationType.isValid("PPL"));
        assertTrue(OptimizationType.isValid("HyDE"));

        // Invalid codes
        assertFalse(OptimizationType.isValid("invalid"));
        assertFalse(OptimizationType.isValid("unknown_type"));
        assertFalse(OptimizationType.isValid(""));
    }

    @Test
    void testGetCode() {
        assertEquals("ppl", OptimizationType.PPL.getCode());
        assertEquals("hyde", OptimizationType.HYDE.getCode());
        assertEquals("rerank", OptimizationType.RERANK.getCode());
        assertEquals("query_expansion", OptimizationType.QUERY_EXPANSION.getCode());
        assertEquals("metadata_filter", OptimizationType.METADATA_FILTER.getCode());
        assertEquals("custom", OptimizationType.CUSTOM.getCode());
    }

    @Test
    void testGetNameEn() {
        assertEquals("Prompt Programming Language", OptimizationType.PPL.getNameEn());
        assertEquals("Hypothetical Document Embeddings", OptimizationType.HYDE.getNameEn());
        assertEquals("Semantic Reranking", OptimizationType.RERANK.getNameEn());
        assertEquals("Query Expansion", OptimizationType.QUERY_EXPANSION.getNameEn());
        assertEquals("Custom Algorithm", OptimizationType.CUSTOM.getNameEn());
    }

    @Test
    void testGetNameZh() {
        assertEquals("提示词编程", OptimizationType.PPL.getNameZh());
        assertEquals("假设性文档嵌入", OptimizationType.HYDE.getNameZh());
        assertEquals("语义重排序", OptimizationType.RERANK.getNameZh());
        assertEquals("查询扩展", OptimizationType.QUERY_EXPANSION.getNameZh());
        assertEquals("自定义算法", OptimizationType.CUSTOM.getNameZh());
    }

    @Test
    void testToString() {
        assertEquals("ppl", OptimizationType.PPL.toString());
        assertEquals("hyde", OptimizationType.HYDE.toString());
        assertEquals("rerank", OptimizationType.RERANK.toString());
        assertEquals("custom", OptimizationType.CUSTOM.toString());
    }

    @Test
    void testAllTypesHaveAllFields() {
        // Ensure all types have non-null names
        for (OptimizationType type : OptimizationType.values()) {
            assertNotNull(type.getCode(), "Code should not be null for " + type);
            assertNotNull(type.getNameEn(), "English name should not be null for " + type);
            assertNotNull(type.getNameZh(), "Chinese name should not be null for " + type);

            assertFalse(type.getCode().isEmpty(), "Code should not be empty for " + type);
            assertFalse(type.getNameEn().isEmpty(), "English name should not be empty for " + type);
            assertFalse(type.getNameZh().isEmpty(), "Chinese name should not be empty for " + type);
        }
    }

    @Test
    void testEnumValues() {
        // Test that we have the expected number of enum values
        OptimizationType[] types = OptimizationType.values();
        assertEquals(14, types.length, "Should have 14 optimization types");
    }

    @Test
    void testEnumValueOf() {
        // Test valueOf method
        assertEquals(OptimizationType.PPL, OptimizationType.valueOf("PPL"));
        assertEquals(OptimizationType.HYDE, OptimizationType.valueOf("HYDE"));
        assertEquals(OptimizationType.RERANK, OptimizationType.valueOf("RERANK"));
        assertEquals(OptimizationType.CUSTOM, OptimizationType.valueOf("CUSTOM"));
    }

    @Test
    void testEnumValueOf_Invalid() {
        // Test valueOf with invalid name
        assertThrows(IllegalArgumentException.class, () -> {
            OptimizationType.valueOf("INVALID_TYPE");
        });
    }

    @Test
    void testCodeConsistency() {
        // Ensure code matches the toString output
        for (OptimizationType type : OptimizationType.values()) {
            assertEquals(type.getCode(), type.toString(),
                    "Code and toString should match for " + type);
        }
    }
}

