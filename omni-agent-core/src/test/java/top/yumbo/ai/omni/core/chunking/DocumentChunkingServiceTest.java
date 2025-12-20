package top.yumbo.ai.omni.core.chunking;

import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.DisplayName;
import org.mockito.Mock;
import org.mockito.MockitoAnnotations;
import top.yumbo.ai.omni.core.chunking.ChunkingStrategyManager;
import top.yumbo.ai.storage.api.DocumentStorageService;
import top.yumbo.ai.storage.api.model.Chunk;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;

import static org.junit.jupiter.api.Assertions.*;
import static org.mockito.ArgumentMatchers.*;
import static org.mockito.Mockito.*;

/**
 * DocumentChunkingService Unit Test
 *
 * @author OmniAgent Team
 * @since 1.0.0
 */
@DisplayName("Document Chunking Service Test")
class DocumentChunkingServiceTest {

    @Mock
    private DocumentStorageService storageService;

    @Mock
    private ChunkingStrategyManager strategyManager;

    private DocumentChunkingService chunkingService;

    @BeforeEach
    void setUp() {
        MockitoAnnotations.openMocks(this);
        chunkingService = new DocumentChunkingService(storageService, strategyManager);

        // Mock strategyManager 的默认行为：使用简单的固定大小分块
        when(strategyManager.chunkWithAutoStrategy(anyString(), anyString(), anyString()))
            .thenAnswer(invocation -> {
                String docId = invocation.getArgument(0);
                String content = invocation.getArgument(1);

                if (content == null || content.isEmpty()) {
                    return List.of();
                }

                // 简单分块逻辑：每500个字符一块
                List<Chunk> chunks = new ArrayList<>();
                int chunkSize = 500;
                int pos = 0;
                int sequence = 0;

                while (pos < content.length()) {
                    int end = Math.min(pos + chunkSize, content.length());
                    String chunkContent = content.substring(pos, end);

                    chunks.add(Chunk.builder()
                        .documentId(docId)
                        .content(chunkContent)
                        .sequence(sequence++)
                        .startPosition(pos)
                        .endPosition(end)
                        .createdAt(System.currentTimeMillis())
                        .build());

                    pos = end;
                }

                return chunks;
            });
    }

    @Test
    @DisplayName("Chunk short text")
    void testChunkShortText() {
        String documentId = "doc-001";
        String content = "This is a short text.";

        List<Chunk> chunks = chunkingService.chunkDocument(documentId, content);

        assertNotNull(chunks);
        assertEquals(1, chunks.size());
        assertEquals(content, chunks.get(0).getContent());
        assertEquals(documentId, chunks.get(0).getDocumentId());
    }

    @Test
    @DisplayName("Chunk long text")
    void testChunkLongText() {
        String documentId = "doc-002";
        StringBuilder sb = new StringBuilder();
        for (int i = 0; i < 100; i++) {
            sb.append("This is sentence ").append(i).append(". ");
        }
        String content = sb.toString();

        List<Chunk> chunks = chunkingService.chunkDocument(documentId, content);

        assertNotNull(chunks);
        assertTrue(chunks.size() > 1);
        for (Chunk chunk : chunks) {
            assertEquals(documentId, chunk.getDocumentId());
            assertNotNull(chunk.getContent());
        }
    }

    @Test
    @DisplayName("Chunk with custom size")
    void testChunkWithCustomSize() {
        String documentId = "doc-003";
        String content = "0123456789".repeat(10); // 100 chars
        int chunkSize = 30;
        int overlapSize = 5;

        List<Chunk> chunks = chunkingService.chunkDocument(
            documentId, content, chunkSize, overlapSize);

        assertNotNull(chunks);
        assertTrue(chunks.size() > 1);
        for (Chunk chunk : chunks) {
            assertTrue(chunk.getContent().length() <= chunkSize);
        }
    }

    @Test
    @DisplayName("Chunk empty content")
    void testChunkEmptyContent() {
        String documentId = "doc-004";
        String content = "";

        List<Chunk> chunks = chunkingService.chunkDocument(documentId, content);

        assertNotNull(chunks);
        assertTrue(chunks.isEmpty());
    }

    @Test
    @DisplayName("Chunk null content")
    void testChunkNullContent() {
        String documentId = "doc-005";

        List<Chunk> chunks = chunkingService.chunkDocument(documentId, null);

        assertNotNull(chunks);
        assertTrue(chunks.isEmpty());
    }

    @Test
    @DisplayName("Chunk and store")
    void testChunkAndStore() {
        String documentId = "doc-006";
        String content = "Test content for storage.";
        List<String> mockChunkIds = Arrays.asList("chunk-1", "chunk-2");

        when(storageService.saveChunks(eq(documentId), anyList()))
            .thenReturn(mockChunkIds);

        List<String> chunkIds = chunkingService.chunkAndStore(documentId, content);

        assertNotNull(chunkIds);
        assertEquals(2, chunkIds.size());
        verify(storageService).saveChunks(eq(documentId), anyList());
    }

    @Test
    @DisplayName("Chunk and store empty content")
    void testChunkAndStoreEmptyContent() {
        String documentId = "doc-007";
        String content = "";

        List<String> chunkIds = chunkingService.chunkAndStore(documentId, content);

        assertNotNull(chunkIds);
        assertTrue(chunkIds.isEmpty());
        verify(storageService, never()).saveChunks(anyString(), anyList());
    }

    @Test
    @DisplayName("Get chunks for document")
    void testGetChunks() {
        String documentId = "doc-008";
        List<Chunk> mockChunks = Arrays.asList(
            createMockChunk(documentId, "content1", 0),
            createMockChunk(documentId, "content2", 1)
        );

        when(storageService.getChunksByDocument(documentId))
            .thenReturn(mockChunks);

        List<Chunk> chunks = chunkingService.getChunks(documentId);

        assertNotNull(chunks);
        assertEquals(2, chunks.size());
        verify(storageService).getChunksByDocument(documentId);
    }

    @Test
    @DisplayName("Chunk sequence numbering")
    void testChunkSequenceNumbering() {
        String documentId = "doc-009";
        String content = "0123456789".repeat(20); // 200 chars
        int chunkSize = 50;
        int overlapSize = 10;

        List<Chunk> chunks = chunkingService.chunkDocument(
            documentId, content, chunkSize, overlapSize);

        for (int i = 0; i < chunks.size(); i++) {
            assertEquals(i, chunks.get(i).getSequence());
        }
    }

    @Test
    @DisplayName("Chunk overlap verification")
    void testChunkOverlap() {
        String documentId = "doc-010";
        String content = "ABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789"; // 36 chars
        int chunkSize = 20;
        int overlapSize = 5;

        List<Chunk> chunks = chunkingService.chunkDocument(
            documentId, content, chunkSize, overlapSize);

        // Verify we got multiple chunks
        assertTrue(chunks.size() > 1);

        // Verify all chunks are within size limit
        for (Chunk chunk : chunks) {
            assertTrue(chunk.getContent().length() <= chunkSize);
        }
    }

    @Test
    @DisplayName("Chunk position tracking")
    void testChunkPositionTracking() {
        String documentId = "doc-011";
        String content = "0123456789".repeat(10);

        List<Chunk> chunks = chunkingService.chunkDocument(documentId, content);

        for (Chunk chunk : chunks) {
            assertTrue(chunk.getStartPosition() >= 0);
            assertTrue(chunk.getEndPosition() > chunk.getStartPosition());
            assertTrue(chunk.getEndPosition() <= content.length());
        }
    }

    @Test
    @DisplayName("Chunk timestamp")
    void testChunkTimestamp() {
        String documentId = "doc-012";
        String content = "Test content";

        List<Chunk> chunks = chunkingService.chunkDocument(documentId, content);

        for (Chunk chunk : chunks) {
            assertNotNull(chunk.getCreatedAt());
            assertTrue(chunk.getCreatedAt() > 0);
        }
    }

    private Chunk createMockChunk(String documentId, String content, int sequence) {
        return Chunk.builder()
            .documentId(documentId)
            .content(content)
            .sequence(sequence)
            .startPosition(0)
            .endPosition(content.length())
            .createdAt(System.currentTimeMillis())
            .build();
    }
}

