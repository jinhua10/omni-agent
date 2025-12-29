package top.yumbo.ai.omni.ai.starter.impl;

import com.fasterxml.jackson.databind.ObjectMapper;
import com.fasterxml.jackson.databind.SerializationFeature;
import com.fasterxml.jackson.datatype.jsr310.JavaTimeModule;
import lombok.extern.slf4j.Slf4j;
import org.springframework.scheduling.annotation.Async;
import org.springframework.stereotype.Service;
import reactor.core.publisher.Flux;
import reactor.core.publisher.Mono;
import reactor.core.scheduler.Schedulers;
import top.yumbo.ai.omni.ai.archive.model.AICallArchive;
import top.yumbo.ai.omni.ai.archive.AICallArchiveService;

import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.Paths;
import java.nio.file.StandardOpenOption;
import java.time.Instant;
import java.time.LocalDate;
import java.time.ZoneId;
import java.time.format.DateTimeFormatter;
import java.util.*;
import java.util.concurrent.ConcurrentHashMap;
import java.util.stream.Collectors;
import java.util.stream.Stream;

/**
 * 基于文件存储的AI调用归档服务实现
 *
 * <p>归档结构：data/ai-archives/YYYY-MM-DD/archive_timestamp_id.json</p>
 *
 * @author OmniAgent Team
 * @since 3.0.0
 */
@Slf4j
@Service
public class FileAICallArchiveService implements AICallArchiveService {

    private static final String BASE_PATH = "data/ai-archives";
    private static final DateTimeFormatter DATE_FORMATTER = DateTimeFormatter.ofPattern("yyyy-MM-dd");

    private final ObjectMapper objectMapper;
    private final Path basePath;

    // 内存索引（可选，用于快速查询）
    private final Map<String, AICallArchive> memoryIndex = new ConcurrentHashMap<>();
    private final int MAX_MEMORY_INDEX = 1000; // 最多缓存1000条

    public FileAICallArchiveService() {
        this.basePath = Paths.get(BASE_PATH);
        this.objectMapper = new ObjectMapper();
        this.objectMapper.registerModule(new JavaTimeModule());
        this.objectMapper.disable(SerializationFeature.WRITE_DATES_AS_TIMESTAMPS);
        this.objectMapper.enable(SerializationFeature.INDENT_OUTPUT);

        try {
            Files.createDirectories(basePath);
            log.info("✅ AI调用归档服务初始化完成，路径: {}", basePath.toAbsolutePath());
        } catch (IOException e) {
            log.error("❌ 创建归档目录失败", e);
        }
    }

    @Override
    @Async("aiArchiveExecutor")
    public Mono<String> archiveAsync(AICallArchive archive) {
        return Mono.fromCallable(() -> archive(archive))
                .subscribeOn(Schedulers.boundedElastic())
                .doOnSuccess(id -> log.debug("✅ 异步归档完成: {}", id))
                .doOnError(e -> log.error("❌ 异步归档失败", e));
    }

    @Override
    public Mono<String> archiveStreamAsync(AICallArchive archive, Flux<String> responseFlux) {
        return responseFlux
                .collect(StringBuilder::new, StringBuilder::append)
                .map(StringBuilder::toString)
                .flatMap(fullResponse -> {
                    // 设置完整响应
                    archive.setResponseText(fullResponse);
                    return archiveAsync(archive);
                })
                .doOnSuccess(id -> log.debug("✅ 流式调用归档完成: {}, 响应长度: {}",
                        id, archive.getResponseText() != null ? archive.getResponseText().length() : 0))
                .doOnError(e -> log.error("❌ 流式调用归档失败", e));
    }

    @Override
    public String archive(AICallArchive archive) {
        try {
            // 确保有归档ID
            if (archive.getArchiveId() == null) {
                archive.setArchiveId(AICallArchive.generateArchiveId());
            }

            // 确保有时间戳
            if (archive.getTimestamp() == null) {
                archive.setTimestamp(System.currentTimeMillis());
            }

            // 确保有ISO时间
            if (archive.getCallTime() == null) {
                archive.setCallTime(AICallArchive.getCurrentTimeISO());
            }

            // 根据日期创建目录
            String dateStr = getDateString(archive.getTimestamp());
            Path dateDir = basePath.resolve(dateStr);
            Files.createDirectories(dateDir);

            // 保存JSON文件
            Path archiveFile = dateDir.resolve(archive.getArchiveId() + ".json");
            String json = objectMapper.writeValueAsString(archive);
            Files.writeString(archiveFile, json, StandardOpenOption.CREATE, StandardOpenOption.TRUNCATE_EXISTING);

            // 添加到内存索引
            if (memoryIndex.size() < MAX_MEMORY_INDEX) {
                memoryIndex.put(archive.getArchiveId(), archive);
            }

            log.debug("💾 AI调用已归档: {}, 文件: {}", archive.getArchiveId(), archiveFile);
            return archive.getArchiveId();

        } catch (Exception e) {
            log.error("❌ 归档失败: archiveId={}", archive.getArchiveId(), e);
            return null;
        }
    }

    @Override
    public Optional<AICallArchive> getArchive(String archiveId) {
        // 先从内存索引查找
        if (memoryIndex.containsKey(archiveId)) {
            return Optional.of(memoryIndex.get(archiveId));
        }

        // 从文件查找
        try {
            // 遍历所有日期目录
            try (Stream<Path> dateDirs = Files.list(basePath)) {
                return dateDirs
                        .filter(Files::isDirectory)
                        .flatMap(dateDir -> {
                            Path archiveFile = dateDir.resolve(archiveId + ".json");
                            if (Files.exists(archiveFile)) {
                                try {
                                    String json = Files.readString(archiveFile);
                                    AICallArchive archive = objectMapper.readValue(json, AICallArchive.class);
                                    return Stream.of(archive);
                                } catch (IOException e) {
                                    log.error("读取归档文件失败: {}", archiveFile, e);
                                }
                            }
                            return Stream.empty();
                        })
                        .findFirst();
            }
        } catch (IOException e) {
            log.error("查找归档失败: {}", archiveId, e);
        }

        return Optional.empty();
    }

    @Override
    public List<AICallArchive> queryByTimeRange(Long startTime, Long endTime, Integer limit) {
        List<AICallArchive> results = new ArrayList<>();

        try {
            // 计算日期范围
            LocalDate startDate = Instant.ofEpochMilli(startTime).atZone(ZoneId.systemDefault()).toLocalDate();
            LocalDate endDate = Instant.ofEpochMilli(endTime).atZone(ZoneId.systemDefault()).toLocalDate();

            // 遍历日期范围
            LocalDate current = startDate;
            while (!current.isAfter(endDate) && results.size() < limit) {
                String dateStr = current.format(DATE_FORMATTER);
                Path dateDir = basePath.resolve(dateStr);

                if (Files.exists(dateDir)) {
                    try (Stream<Path> files = Files.list(dateDir)) {
                        files.filter(p -> p.toString().endsWith(".json"))
                                .forEach(file -> {
                                    if (results.size() < limit) {
                                        try {
                                            String json = Files.readString(file);
                                            AICallArchive archive = objectMapper.readValue(json, AICallArchive.class);
                                            if (archive.getTimestamp() >= startTime && archive.getTimestamp() <= endTime) {
                                                results.add(archive);
                                            }
                                        } catch (IOException e) {
                                            log.error("读取归档文件失败: {}", file, e);
                                        }
                                    }
                                });
                    }
                }

                current = current.plusDays(1);
            }
        } catch (Exception e) {
            log.error("按时间范围查询失败", e);
        }

        return results.stream()
                .sorted(Comparator.comparing(AICallArchive::getTimestamp).reversed())
                .limit(limit)
                .collect(Collectors.toList());
    }

    @Override
    public List<AICallArchive> queryByModel(String model, Integer limit) {
        return queryAll(limit).stream()
                .filter(archive -> model.equals(archive.getModel()))
                .collect(Collectors.toList());
    }

    @Override
    public List<AICallArchive> queryByDocument(String documentId) {
        return queryAll(1000).stream()
                .filter(archive -> documentId.equals(archive.getRelatedDocumentId()))
                .collect(Collectors.toList());
    }

    @Override
    public ArchiveStatistics getStatistics() {
        ArchiveStatistics stats = new ArchiveStatistics();

        List<AICallArchive> allArchives = queryAll(10000);

        stats.setTotalCalls((long) allArchives.size());
        stats.setSuccessCalls(allArchives.stream().filter(a -> Boolean.TRUE.equals(a.getSuccess())).count());
        stats.setFailedCalls(allArchives.stream().filter(a -> Boolean.FALSE.equals(a.getSuccess())).count());
        stats.setStreamCalls(allArchives.stream().filter(a -> Boolean.TRUE.equals(a.getIsStream())).count());

        long totalTokens = allArchives.stream()
                .filter(a -> a.getTokenUsage() != null && a.getTokenUsage().getTotalTokens() != null)
                .mapToLong(a -> a.getTokenUsage().getTotalTokens())
                .sum();
        stats.setTotalTokens(totalTokens);

        OptionalDouble avgDuration = allArchives.stream()
                .filter(a -> a.getDurationMs() != null)
                .mapToLong(AICallArchive::getDurationMs)
                .average();
        stats.setAvgDurationMs(avgDuration.isPresent() ? avgDuration.getAsDouble() : 0.0);

        return stats;
    }

    @Override
    public int cleanOldArchives(Long olderThan) {
        int deleted = 0;

        try {
            LocalDate cutoffDate = Instant.ofEpochMilli(olderThan).atZone(ZoneId.systemDefault()).toLocalDate();

            try (Stream<Path> dateDirs = Files.list(basePath)) {
                for (Path dateDir : dateDirs.filter(Files::isDirectory).collect(Collectors.toList())) {
                    try {
                        String dirName = dateDir.getFileName().toString();
                        LocalDate dirDate = LocalDate.parse(dirName, DATE_FORMATTER);

                        if (dirDate.isBefore(cutoffDate)) {
                            // 删除整个目录
                            try (Stream<Path> files = Files.walk(dateDir)) {
                                files.sorted(Comparator.reverseOrder())
                                        .forEach(file -> {
                                            try {
                                                Files.delete(file);
                                            } catch (IOException e) {
                                                log.error("删除文件失败: {}", file, e);
                                            }
                                        });
                            }
                            deleted++;
                            log.info("🗑️ 已清理旧归档目录: {}", dateDir);
                        }
                    } catch (Exception e) {
                        log.error("解析目录日期失败: {}", dateDir, e);
                    }
                }
            }
        } catch (Exception e) {
            log.error("清理旧归档失败", e);
        }

        return deleted;
    }

    // ========== 辅助方法 ==========

    private String getDateString(Long timestamp) {
        return Instant.ofEpochMilli(timestamp)
                .atZone(ZoneId.systemDefault())
                .toLocalDate()
                .format(DATE_FORMATTER);
    }

    private List<AICallArchive> queryAll(Integer limit) {
        List<AICallArchive> results = new ArrayList<>();

        try {
            try (Stream<Path> dateDirs = Files.list(basePath)) {
                for (Path dateDir : dateDirs.filter(Files::isDirectory).collect(Collectors.toList())) {
                    try (Stream<Path> files = Files.list(dateDir)) {
                        files.filter(p -> p.toString().endsWith(".json"))
                                .forEach(file -> {
                                    if (results.size() < limit) {
                                        try {
                                            String json = Files.readString(file);
                                            AICallArchive archive = objectMapper.readValue(json, AICallArchive.class);
                                            results.add(archive);
                                        } catch (IOException e) {
                                            log.error("读取归档文件失败: {}", file, e);
                                        }
                                    }
                                });
                    }
                }
            }
        } catch (Exception e) {
            log.error("查询所有归档失败", e);
        }

        return results.stream()
                .sorted(Comparator.comparing(AICallArchive::getTimestamp).reversed())
                .limit(limit)
                .collect(Collectors.toList());
    }
}

