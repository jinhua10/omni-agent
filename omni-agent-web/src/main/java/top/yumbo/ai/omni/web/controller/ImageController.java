package top.yumbo.ai.omni.web.controller;

import lombok.Data;
import lombok.RequiredArgsConstructor;
import lombok.extern.slf4j.Slf4j;
import org.springframework.core.io.ByteArrayResource;
import org.springframework.core.io.Resource;
import org.springframework.http.HttpHeaders;
import org.springframework.http.MediaType;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.*;
import top.yumbo.ai.omni.core.image.ImageStorageService;
import top.yumbo.ai.omni.storage.api.model.Image;

import java.util.List;
import java.util.stream.Collectors;

/**
 * 图片访问控制器
 * (Image Access Controller)
 *
 * <p>提供图片访问、列表、缓存控制功能</p>
 *
 * @author OmniAgent Team
 * @since 1.0.0
 */
@Slf4j
@RestController
@RequestMapping("/api/images")
@RequiredArgsConstructor
public class ImageController {

    private final ImageStorageService imageStorageService;

    /**
     * 获取图片
     * GET /api/images/{imageId}
     */
    @GetMapping("/{imageId}")
    public ResponseEntity<Resource> getImage(@PathVariable String imageId) {

        try {
            log.debug("获取图片: imageId={}", imageId);

            // 读取图片数据
            Image image = imageStorageService.getImage(imageId)
                .orElseThrow(() -> new RuntimeException("Image not found"));

            byte[] imageData = image.getData();

            // 转换为资源
            ByteArrayResource resource = new ByteArrayResource(imageData);

            // 确定 MIME 类型
            String extension = image.getFormat() != null ? image.getFormat() : "png";
            MediaType mediaType = getMediaType(extension);

            log.debug("图片读取成功: size={} bytes, type={}", imageData.length, mediaType);

            return ResponseEntity.ok()
                    .contentType(mediaType)
                    .contentLength(imageData.length)
                    .header(HttpHeaders.CACHE_CONTROL, "max-age=31536000") // 缓存1年
                    .body(resource);

        } catch (Exception e) {
            log.error("获取图片失败: imageId={}", imageId, e);
            return ResponseEntity.notFound().build();
        }
    }

    /**
     * 列出文档的所有图片
     * GET /api/images/list/{documentId}
     */
    @GetMapping("/list/{documentId}")
    public ResponseEntity<List<ImageInfo>> listImages(@PathVariable String documentId) {
        try {
            log.debug("列出文档图片: documentId={}", documentId);

            List<Image> images = imageStorageService.getImagesByDocument(documentId);

            // 转换为简单的响应格式
            List<ImageInfo> imageInfos = images.stream()
                .map(img -> {
                    ImageInfo info = new ImageInfo();
                    info.setImageId(img.getId());
                    info.setDocumentId(documentId);
                    info.setFormat(img.getFormat());
                    info.setSize((long) img.getData().length);
                    info.setUrl("/api/images/" + img.getId());
                    return info;
                })
                .collect(Collectors.toList());

            log.debug("找到 {} 个图片", imageInfos.size());

            return ResponseEntity.ok(imageInfos);
        } catch (Exception e) {
            log.error("列出图片失败: documentId={}", documentId, e);
            return ResponseEntity.internalServerError().build();
        }
    }

    /**
     * 根据文件扩展名确定 MIME 类型
     */
    private MediaType getMediaType(String extension) {
        return switch (extension.toLowerCase()) {
            case "jpg", "jpeg" -> MediaType.IMAGE_JPEG;
            case "png" -> MediaType.IMAGE_PNG;
            case "gif" -> MediaType.IMAGE_GIF;
            case "svg" -> MediaType.valueOf("image/svg+xml");
            case "webp" -> MediaType.valueOf("image/webp");
            case "bmp" -> MediaType.valueOf("image/bmp");
            default -> MediaType.APPLICATION_OCTET_STREAM;
        };
    }

    /**
     * 图片信息 DTO
     */
    @Data
    public static class ImageInfo {
        private String imageId;
        private String documentId;
        private String format;
        private Long size;
        private String url;
    }
}

