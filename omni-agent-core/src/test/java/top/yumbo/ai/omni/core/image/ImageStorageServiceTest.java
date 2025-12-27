package top.yumbo.ai.omni.core.image;

import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.DisplayName;
import org.junit.jupiter.api.Test;
import org.mockito.ArgumentCaptor;
import top.yumbo.ai.omni.storage.api.DocumentStorageService;
import top.yumbo.ai.omni.storage.api.model.Image;

import java.util.Arrays;
import java.util.List;
import java.util.Optional;

import static org.junit.jupiter.api.Assertions.*;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.ArgumentMatchers.eq;
import static org.mockito.Mockito.*;

/**
 * ImageStorageService 单元测试
 *
 * @author OmniAgent Team
 * @since 1.0.0
 */
@DisplayName("ImageStorageService Tests")
class ImageStorageServiceTest {

    private DocumentStorageService mockStorage;
    private ImageStorageService imageService;

    @BeforeEach
    void setUp() {
        mockStorage = mock(DocumentStorageService.class);
        imageService = new ImageStorageService(mockStorage);
    }

    @Test
    @DisplayName("保存图像 - 成功")
    void testSaveImage_Success() {
        // Given
        String documentId = "doc-123";
        byte[] imageData = new byte[]{1, 2, 3, 4, 5};
        String format = "png";
        String expectedImageId = "img-123";

        when(mockStorage.saveImage(eq(documentId), any(Image.class)))
            .thenReturn(expectedImageId);

        // When
        String result = imageService.saveImage(documentId, imageData, format);

        // Then
        assertEquals(expectedImageId, result);
        verify(mockStorage).saveImage(eq(documentId), any(Image.class));

        // 验证传入的Image对象
        ArgumentCaptor<Image> imageCaptor = ArgumentCaptor.forClass(Image.class);
        verify(mockStorage).saveImage(eq(documentId), imageCaptor.capture());
        Image capturedImage = imageCaptor.getValue();

        assertEquals(documentId, capturedImage.getDocumentId());
        assertArrayEquals(imageData, capturedImage.getData());
        assertEquals(format, capturedImage.getFormat());
        assertTrue(capturedImage.getCreatedAt() > 0);
    }

    @Test
    @DisplayName("保存图像 - 空数据")
    void testSaveImage_EmptyData() {
        // Given
        String documentId = "doc-123";
        byte[] emptyData = new byte[0];
        String format = "png";

        // When
        String result = imageService.saveImage(documentId, emptyData, format);

        // Then
        assertNull(result);
        verify(mockStorage, never()).saveImage(any(), any());
    }

    @Test
    @DisplayName("保存图像 - null数据")
    void testSaveImage_NullData() {
        // Given
        String documentId = "doc-123";
        String format = "png";

        // When
        String result = imageService.saveImage(documentId, null, format);

        // Then
        assertNull(result);
        verify(mockStorage, never()).saveImage(any(), any());
    }

    @Test
    @DisplayName("保存图像 - 存储异常")
    void testSaveImage_StorageException() {
        // Given
        String documentId = "doc-123";
        byte[] imageData = new byte[]{1, 2, 3};
        String format = "jpg";

        when(mockStorage.saveImage(any(), any()))
            .thenThrow(new RuntimeException("Storage error"));

        // When
        String result = imageService.saveImage(documentId, imageData, format);

        // Then
        assertNull(result);
    }

    @Test
    @DisplayName("获取图像 - 成功")
    void testGetImage_Success() {
        // Given
        String imageId = "img-123";
        Image expectedImage = Image.builder()
            .id(imageId)
            .documentId("doc-123")
            .data(new byte[]{1, 2, 3})
            .format("png")
            .createdAt(System.currentTimeMillis())
            .build();

        when(mockStorage.getImage(imageId))
            .thenReturn(Optional.of(expectedImage));

        // When
        Optional<Image> result = imageService.getImage(imageId);

        // Then
        assertTrue(result.isPresent());
        assertEquals(expectedImage, result.get());
        verify(mockStorage).getImage(imageId);
    }

    @Test
    @DisplayName("获取图像 - 不存在")
    void testGetImage_NotFound() {
        // Given
        String imageId = "nonexistent";

        when(mockStorage.getImage(imageId))
            .thenReturn(Optional.empty());

        // When
        Optional<Image> result = imageService.getImage(imageId);

        // Then
        assertFalse(result.isPresent());
        verify(mockStorage).getImage(imageId);
    }

    @Test
    @DisplayName("获取图像 - 异常")
    void testGetImage_Exception() {
        // Given
        String imageId = "img-123";

        when(mockStorage.getImage(imageId))
            .thenThrow(new RuntimeException("Get error"));

        // When
        Optional<Image> result = imageService.getImage(imageId);

        // Then
        assertFalse(result.isPresent());
    }

    @Test
    @DisplayName("获取文档的所有图像 - 成功")
    void testGetImagesByDocument_Success() {
        // Given
        String documentId = "doc-123";
        List<Image> expectedImages = Arrays.asList(
            Image.builder().id("img-1").documentId(documentId).build(),
            Image.builder().id("img-2").documentId(documentId).build()
        );

        when(mockStorage.getImagesByDocument(documentId))
            .thenReturn(expectedImages);

        // When
        List<Image> result = imageService.getImagesByDocument(documentId);

        // Then
        assertEquals(2, result.size());
        assertEquals(expectedImages, result);
        verify(mockStorage).getImagesByDocument(documentId);
    }

    @Test
    @DisplayName("获取文档的所有图像 - 无图像")
    void testGetImagesByDocument_Empty() {
        // Given
        String documentId = "doc-no-images";

        when(mockStorage.getImagesByDocument(documentId))
            .thenReturn(List.of());

        // When
        List<Image> result = imageService.getImagesByDocument(documentId);

        // Then
        assertTrue(result.isEmpty());
        verify(mockStorage).getImagesByDocument(documentId);
    }

    @Test
    @DisplayName("获取文档的所有图像 - 异常")
    void testGetImagesByDocument_Exception() {
        // Given
        String documentId = "doc-123";

        when(mockStorage.getImagesByDocument(documentId))
            .thenThrow(new RuntimeException("Query error"));

        // When
        List<Image> result = imageService.getImagesByDocument(documentId);

        // Then
        assertTrue(result.isEmpty());
    }

    @Test
    @DisplayName("删除图像 - 成功")
    void testDeleteImage_Success() {
        // Given
        String imageId = "img-123";

        doNothing().when(mockStorage).deleteImage(imageId);

        // When
        imageService.deleteImage(imageId);

        // Then
        verify(mockStorage).deleteImage(imageId);
    }

    @Test
    @DisplayName("删除图像 - 异常")
    void testDeleteImage_Exception() {
        // Given
        String imageId = "img-123";

        doThrow(new RuntimeException("Delete error"))
            .when(mockStorage).deleteImage(imageId);

        // When & Then - 不应抛出异常
        assertDoesNotThrow(() -> imageService.deleteImage(imageId));
    }

    @Test
    @DisplayName("删除文档的所有图像 - 成功")
    void testDeleteImagesByDocument_Success() {
        // Given
        String documentId = "doc-123";

        doNothing().when(mockStorage).deleteImagesByDocument(documentId);

        // When
        imageService.deleteImagesByDocument(documentId);

        // Then
        verify(mockStorage).deleteImagesByDocument(documentId);
    }

    @Test
    @DisplayName("删除文档的所有图像 - 异常")
    void testDeleteImagesByDocument_Exception() {
        // Given
        String documentId = "doc-123";

        doThrow(new RuntimeException("Delete error"))
            .when(mockStorage).deleteImagesByDocument(documentId);

        // When & Then - 不应抛出异常
        assertDoesNotThrow(() -> imageService.deleteImagesByDocument(documentId));
    }

    @Test
    @DisplayName("测试不同图片格式")
    void testSaveImage_DifferentFormats() {
        // Given
        String documentId = "doc-123";
        byte[] imageData = new byte[]{1, 2, 3};
        String[] formats = {"png", "jpg", "jpeg", "gif", "bmp", "webp"};

        when(mockStorage.saveImage(any(), any()))
            .thenReturn("img-id");

        // When & Then
        for (String format : formats) {
            String result = imageService.saveImage(documentId, imageData, format);
            assertNotNull(result);
        }

        verify(mockStorage, times(formats.length)).saveImage(any(), any());
    }

    @Test
    @DisplayName("测试大图片数据")
    void testSaveImage_LargeData() {
        // Given
        String documentId = "doc-123";
        byte[] largeImageData = new byte[10 * 1024 * 1024]; // 10MB
        String format = "png";

        when(mockStorage.saveImage(any(), any()))
            .thenReturn("img-large");

        // When
        String result = imageService.saveImage(documentId, largeImageData, format);

        // Then
        assertNotNull(result);
        verify(mockStorage).saveImage(eq(documentId), any(Image.class));
    }
}


