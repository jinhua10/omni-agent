package top.yumbo.ai.omni.core.ppl;

import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.DisplayName;
import org.junit.jupiter.api.Test;
import org.mockito.ArgumentCaptor;
import top.yumbo.ai.omni.core.optimization.RAGOptimizationService;
import top.yumbo.ai.storage.api.DocumentStorageService;
import top.yumbo.ai.storage.api.model.PPLData;

import java.util.Optional;

import static org.junit.jupiter.api.Assertions.*;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.ArgumentMatchers.eq;
import static org.mockito.Mockito.*;

/**
 * PPLStorageService 单元测试
 *
 * @author OmniAgent Team
 * @since 1.0.0
 */
@DisplayName("PPLStorageService Tests")
class PPLStorageServiceTest {

    private DocumentStorageService mockStorage;
    private RAGOptimizationService mockOptimizationService;
    private PPLStorageService pplService;

    @BeforeEach
    void setUp() {
        mockStorage = mock(DocumentStorageService.class);
        mockOptimizationService = mock(RAGOptimizationService.class);
        pplService = new PPLStorageService(mockStorage, mockOptimizationService);
    }

    @Test
    @DisplayName("保存PPL数据 - 成功")
    void testSavePPLData_Success() {
        // Given
        String documentId = "doc-123";
        String content = "SELECT * FROM table";
        String metadata = "query metadata";
        String expectedPplId = "ppl-123";

        when(mockStorage.savePPLData(eq(documentId), any(PPLData.class)))
            .thenReturn(expectedPplId);

        // When
        String result = pplService.savePPLData(documentId, content, metadata);

        // Then
        assertEquals(expectedPplId, result);
        verify(mockStorage).savePPLData(eq(documentId), any(PPLData.class));

        // 验证传入的PPLData对象
        ArgumentCaptor<PPLData> pplCaptor = ArgumentCaptor.forClass(PPLData.class);
        verify(mockStorage).savePPLData(eq(documentId), pplCaptor.capture());
        PPLData capturedPPL = pplCaptor.getValue();

        assertEquals(documentId, capturedPPL.getDocumentId());
        assertTrue(capturedPPL.getAnalyzedAt() > 0);
    }

    @Test
    @DisplayName("保存PPL数据 - 空内容")
    void testSavePPLData_EmptyContent() {
        // Given
        String documentId = "doc-123";
        String emptyContent = "";
        String metadata = "metadata";

        // When
        String result = pplService.savePPLData(documentId, emptyContent, metadata);

        // Then
        assertNull(result);
        verify(mockStorage, never()).savePPLData(any(), any());
    }

    @Test
    @DisplayName("保存PPL数据 - null内容")
    void testSavePPLData_NullContent() {
        // Given
        String documentId = "doc-123";
        String metadata = "metadata";

        // When
        String result = pplService.savePPLData(documentId, null, metadata);

        // Then
        assertNull(result);
        verify(mockStorage, never()).savePPLData(any(), any());
    }

    @Test
    @DisplayName("保存PPL数据 - 空白内容")
    void testSavePPLData_BlankContent() {
        // Given
        String documentId = "doc-123";
        String blankContent = "   ";
        String metadata = "metadata";

        // When
        String result = pplService.savePPLData(documentId, blankContent, metadata);

        // Then
        assertNull(result);
        verify(mockStorage, never()).savePPLData(any(), any());
    }

    @Test
    @DisplayName("保存PPL数据 - 存储异常")
    void testSavePPLData_StorageException() {
        // Given
        String documentId = "doc-123";
        String content = "PPL content";
        String metadata = "metadata";

        when(mockStorage.savePPLData(any(), any()))
            .thenThrow(new RuntimeException("Storage error"));

        // When
        String result = pplService.savePPLData(documentId, content, metadata);

        // Then
        assertNull(result);
    }

    @Test
    @DisplayName("获取PPL数据 - 成功")
    void testGetPPLData_Success() {
        // Given
        String documentId = "doc-123";
        PPLData expectedPPL = PPLData.builder()
            .documentId(documentId)
            .analyzedAt(System.currentTimeMillis())
            .build();

        when(mockStorage.getPPLData(documentId))
            .thenReturn(Optional.of(expectedPPL));

        // When
        Optional<PPLData> result = pplService.getPPLData(documentId);

        // Then
        assertTrue(result.isPresent());
        assertEquals(expectedPPL, result.get());
        verify(mockStorage).getPPLData(documentId);
    }

    @Test
    @DisplayName("获取PPL数据 - 不存在")
    void testGetPPLData_NotFound() {
        // Given
        String documentId = "nonexistent";

        when(mockStorage.getPPLData(documentId))
            .thenReturn(Optional.empty());

        // When
        Optional<PPLData> result = pplService.getPPLData(documentId);

        // Then
        assertFalse(result.isPresent());
        verify(mockStorage).getPPLData(documentId);
    }

    @Test
    @DisplayName("获取PPL数据 - 异常")
    void testGetPPLData_Exception() {
        // Given
        String documentId = "doc-123";

        when(mockStorage.getPPLData(documentId))
            .thenThrow(new RuntimeException("Get error"));

        // When
        Optional<PPLData> result = pplService.getPPLData(documentId);

        // Then
        assertFalse(result.isPresent());
    }

    @Test
    @DisplayName("删除PPL数据 - 成功")
    void testDeletePPLData_Success() {
        // Given
        String documentId = "doc-123";

        doNothing().when(mockStorage).deletePPLData(documentId);

        // When
        pplService.deletePPLData(documentId);

        // Then
        verify(mockStorage).deletePPLData(documentId);
    }

    @Test
    @DisplayName("删除PPL数据 - 异常")
    void testDeletePPLData_Exception() {
        // Given
        String documentId = "doc-123";

        doThrow(new RuntimeException("Delete error"))
            .when(mockStorage).deletePPLData(documentId);

        // When & Then - 不应抛出异常
        assertDoesNotThrow(() -> pplService.deletePPLData(documentId));
    }

    @Test
    @DisplayName("测试长PPL内容")
    void testSavePPLData_LongContent() {
        // Given
        String documentId = "doc-123";
        StringBuilder longContent = new StringBuilder();
        for (int i = 0; i < 10000; i++) {
            longContent.append("SELECT * FROM table").append(i).append("; ");
        }
        String metadata = "metadata";

        when(mockStorage.savePPLData(any(), any()))
            .thenReturn("ppl-long");

        // When
        String result = pplService.savePPLData(documentId, longContent.toString(), metadata);

        // Then
        assertNotNull(result);
        verify(mockStorage).savePPLData(eq(documentId), any(PPLData.class));
    }

    @Test
    @DisplayName("测试特殊字符PPL内容")
    void testSavePPLData_SpecialCharacters() {
        // Given
        String documentId = "doc-123";
        String content = "SELECT * FROM table WHERE name = 'O\\'Brien' AND age > 30; -- comment\n/* multi-line\ncomment */";
        String metadata = "special chars";

        when(mockStorage.savePPLData(any(), any()))
            .thenReturn("ppl-special");

        // When
        String result = pplService.savePPLData(documentId, content, metadata);

        // Then
        assertNotNull(result);
        verify(mockStorage).savePPLData(eq(documentId), any(PPLData.class));
    }

    @Test
    @DisplayName("测试SQL查询PPL")
    void testSavePPLData_SQLQuery() {
        // Given
        String documentId = "doc-sql";
        String sqlContent = "SELECT u.id, u.name, COUNT(o.id) as order_count " +
                           "FROM users u LEFT JOIN orders o ON u.id = o.user_id " +
                           "WHERE u.active = true GROUP BY u.id HAVING order_count > 5";
        String metadata = "complex query";

        when(mockStorage.savePPLData(any(), any()))
            .thenReturn("ppl-sql");

        // When
        String result = pplService.savePPLData(documentId, sqlContent, metadata);

        // Then
        assertNotNull(result);
        verify(mockStorage).savePPLData(eq(documentId), any(PPLData.class));
    }

    @Test
    @DisplayName("测试多次保存同一文档")
    void testSavePPLData_MultipleUpdates() {
        // Given
        String documentId = "doc-123";
        String content1 = "SELECT * FROM table1";
        String content2 = "SELECT * FROM table2";
        String metadata = "metadata";

        when(mockStorage.savePPLData(any(), any()))
            .thenReturn("ppl-1", "ppl-2");

        // When
        String result1 = pplService.savePPLData(documentId, content1, metadata);
        String result2 = pplService.savePPLData(documentId, content2, metadata);

        // Then
        assertEquals("ppl-1", result1);
        assertEquals("ppl-2", result2);
        verify(mockStorage, times(2)).savePPLData(eq(documentId), any(PPLData.class));
    }
}

