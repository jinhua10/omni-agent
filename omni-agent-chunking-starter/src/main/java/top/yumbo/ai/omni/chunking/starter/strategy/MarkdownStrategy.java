package top.yumbo.ai.omni.chunking.starter.strategy;

import lombok.extern.slf4j.Slf4j;
import top.yumbo.ai.omni.chunking.Chunk;
import top.yumbo.ai.omni.chunking.ChunkingConfig;
import top.yumbo.ai.omni.chunking.ChunkingStrategy;
import top.yumbo.ai.omni.chunking.starter.config.ChunkingProperties;

import java.util.ArrayList;
import java.util.List;
import java.util.UUID;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

/**
 * Markdown 分块策略
 *
 * <p>按 Markdown 结构分块，根据标题层级智能切分文档</p>
 *
 * <p>策略特点：</p>
 * <ul>
 *   <li>识别 Markdown 标题（# ## ### 等）</li>
 *   <li>按标题层级组织内容</li>
 *   <li>保持标题和内容的完整性</li>
 *   <li>支持代码块、列表等 Markdown 元素</li>
 * </ul>
 *
 * @author OmniAgent Team
 * @since 1.0.0
 */
@Slf4j
public class MarkdownStrategy implements ChunkingStrategyExecutor {

    private final ChunkingProperties properties;

    // Markdown 标题正则
    private static final Pattern HEADING_PATTERN = Pattern.compile("^(#{1,6})\\s+(.+)$", Pattern.MULTILINE);

    // 默认分块标题层级
    private static final int DEFAULT_SPLIT_LEVEL = 2; // 按 ## 二级标题分块

    // 默认最大分块大小
    private static final int DEFAULT_MAX_CHUNK_SIZE = 2000;

    public MarkdownStrategy(ChunkingProperties properties) {
        this.properties = properties;
    }

    @Override
    public List<Chunk> execute(String documentId, String content, ChunkingConfig config) {
        if (content == null || content.isEmpty()) {
            return new ArrayList<>();
        }

        // 获取配置参数
        int splitLevel = getSplitLevel(config);
        int maxChunkSize = config.getMaxChunkSize() != null ?
                          config.getMaxChunkSize() : DEFAULT_MAX_CHUNK_SIZE;

        log.debug("📋 Markdown 分块: splitLevel={}, maxChunkSize={}", splitLevel, maxChunkSize);

        // 解析 Markdown 结构
        List<MarkdownSection> sections = parseMarkdownSections(content, splitLevel);

        // 转换为分块
        return createChunksFromSections(documentId, sections, maxChunkSize);
    }

    /**
     * 解析 Markdown 章节
     */
    private List<MarkdownSection> parseMarkdownSections(String content, int splitLevel) {
        List<MarkdownSection> sections = new ArrayList<>();
        Matcher matcher = HEADING_PATTERN.matcher(content);

        int lastEnd = 0;
        MarkdownSection currentSection = null;

        while (matcher.find()) {
            int headingLevel = matcher.group(1).length();
            String headingText = matcher.group(2).trim();
            int headingStart = matcher.start();

            // 如果是分块层级的标题，创建新章节
            if (headingLevel <= splitLevel) {
                // 保存上一个章节的内容
                if (currentSection != null && lastEnd < headingStart) {
                    String sectionContent = content.substring(lastEnd, headingStart).trim();
                    currentSection.setContent(sectionContent);
                    sections.add(currentSection);
                }

                // 创建新章节
                currentSection = new MarkdownSection();
                currentSection.setLevel(headingLevel);
                currentSection.setTitle(headingText);
                currentSection.setStartPosition(headingStart);

                lastEnd = matcher.end();
            }
        }

        // 处理最后一个章节
        if (currentSection != null) {
            String sectionContent = content.substring(lastEnd).trim();
            currentSection.setContent(sectionContent);
            sections.add(currentSection);
        }

        // 如果没有找到任何标题，将整个文档作为一个章节
        if (sections.isEmpty()) {
            MarkdownSection section = new MarkdownSection();
            section.setLevel(1);
            section.setTitle("Document");
            section.setContent(content);
            section.setStartPosition(0);
            sections.add(section);
        }

        return sections;
    }

    /**
     * 从章节创建分块
     */
    private List<Chunk> createChunksFromSections(String documentId,
                                                  List<MarkdownSection> sections,
                                                  int maxChunkSize) {
        List<Chunk> chunks = new ArrayList<>();
        int index = 0;

        for (MarkdownSection section : sections) {
            String fullContent = buildSectionContent(section);

            // 如果章节内容超过最大大小，需要进一步分割
            if (fullContent.length() > maxChunkSize) {
                List<Chunk> subChunks = splitLargeSection(documentId, section, maxChunkSize, index);
                chunks.addAll(subChunks);
                index += subChunks.size();
            } else {
                // 创建单个分块
                Chunk chunk = Chunk.builder()
                        .id(UUID.randomUUID().toString())
                        .documentId(documentId)
                        .content(fullContent)
                        .sequence(index++)
                        .startPosition(section.getStartPosition())
                        .endPosition(section.getStartPosition() + fullContent.length())
                        .strategy(ChunkingStrategy.MARKDOWN)
                        .createdAt(System.currentTimeMillis())
                        .build();
                chunks.add(chunk);
            }
        }

        return chunks;
    }

    /**
     * 构建章节内容（包含标题）
     */
    private String buildSectionContent(MarkdownSection section) {
        StringBuilder sb = new StringBuilder();

        // 添加标题
        sb.append("#".repeat(section.getLevel()))
          .append(" ")
          .append(section.getTitle())
          .append("\n\n");

        // 添加内容
        if (section.getContent() != null && !section.getContent().isEmpty()) {
            sb.append(section.getContent());
        }

        return sb.toString().trim();
    }

    /**
     * 分割大章节
     */
    private List<Chunk> splitLargeSection(String documentId, MarkdownSection section,
                                          int maxChunkSize, int startIndex) {
        List<Chunk> chunks = new ArrayList<>();
        String content = section.getContent();

        if (content == null || content.isEmpty()) {
            return chunks;
        }

        // 按段落分割
        String[] paragraphs = content.split("\\n\\s*\\n");
        StringBuilder currentChunk = new StringBuilder();
        String header = "#".repeat(section.getLevel()) + " " + section.getTitle() + "\n\n";
        currentChunk.append(header);

        int chunkIndex = startIndex;
        int position = section.getStartPosition();

        for (String paragraph : paragraphs) {
            String trimmed = paragraph.trim();
            if (trimmed.isEmpty()) {
                continue;
            }

            // 检查添加这个段落是否会超过限制
            if (currentChunk.length() + trimmed.length() + 2 > maxChunkSize &&
                currentChunk.length() > header.length()) {

                // 保存当前分块
                String chunkContent = currentChunk.toString().trim();
                chunks.add(Chunk.builder()
                        .id(UUID.randomUUID().toString())
                        .documentId(documentId)
                        .content(chunkContent)
                        .sequence(chunkIndex++)
                        .startPosition(position)
                        .endPosition(position + chunkContent.length())
                        .strategy(ChunkingStrategy.MARKDOWN)
                        .createdAt(System.currentTimeMillis())
                        .build());

                // 重置为新分块（保留标题）
                currentChunk = new StringBuilder(header);
                position += chunkContent.length();
            }

            // 添加段落
            if (currentChunk.length() > header.length()) {
                currentChunk.append("\n\n");
            }
            currentChunk.append(trimmed);
        }

        // 保存最后一个分块
        if (currentChunk.length() > header.length()) {
            String chunkContent = currentChunk.toString().trim();
            chunks.add(Chunk.builder()
                    .id(UUID.randomUUID().toString())
                    .documentId(documentId)
                    .content(chunkContent)
                    .sequence(chunkIndex)
                    .startPosition(position)
                    .endPosition(position + chunkContent.length())
                    .strategy(ChunkingStrategy.MARKDOWN)
                    .createdAt(System.currentTimeMillis())
                    .build());
        }

        return chunks;
    }

    /**
     * 获取分块层级
     *
     * <p>默认使用二级标题（##）分块</p>
     */
    private int getSplitLevel(ChunkingConfig config) {
        // 使用默认层级
        return DEFAULT_SPLIT_LEVEL;
    }

    /**
     * Markdown 章节数据结构
     */
    private static class MarkdownSection {
        private int level;
        private String title;
        private String content;
        private int startPosition;

        public int getLevel() {
            return level;
        }

        public void setLevel(int level) {
            this.level = level;
        }

        public String getTitle() {
            return title;
        }

        public void setTitle(String title) {
            this.title = title;
        }

        public String getContent() {
            return content;
        }

        public void setContent(String content) {
            this.content = content;
        }

        public int getStartPosition() {
            return startPosition;
        }

        public void setStartPosition(int startPosition) {
            this.startPosition = startPosition;
        }
    }
}

