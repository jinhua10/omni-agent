package top.yumbo.ai.omni.document.processor.starter.processor;

import lombok.extern.slf4j.Slf4j;
import org.apache.poi.hssf.usermodel.*;
import org.apache.poi.ss.usermodel.*;
import org.apache.poi.xssf.usermodel.*;
import top.yumbo.ai.omni.document.processor.*;
import top.yumbo.ai.omni.document.processor.starter.config.DocumentProcessorProperties;

import java.io.InputStream;
import java.text.DecimalFormat;
import java.util.*;

/**
 * Excel 文档处理器（增强版）
 *
 * <p>支持 .xls 和 .xlsx 格式</p>
 * <p>功能：Markdown 表格转换、多种单元格类型处理、公式计算</p>
 *
 * @author OmniAgent Team
 * @since 1.0.0
 */
@Slf4j
public class ExcelProcessor implements DocumentProcessor {

    private final DocumentProcessorProperties properties;

    private static final int MAX_ROWS_PER_SHEET = 1000;
    private static final int MAX_COLS_PER_SHEET = 50;

    public ExcelProcessor(DocumentProcessorProperties properties) {
        this.properties = properties;
    }

    @Override
    public ProcessedDocument process(String documentId, InputStream input) throws ProcessorException {
        try {
            log.debug("📄 开始处理 Excel 文档: {}", documentId);

            Workbook workbook = WorkbookFactory.create(input);
            StringBuilder text = new StringBuilder();

            int sheetCount = workbook.getNumberOfSheets();
            Map<String, Object> metadata = new HashMap<>();
            metadata.put("totalSheets", sheetCount);

            // 处理每个工作表
            for (int i = 0; i < sheetCount; i++) {
                Sheet sheet = workbook.getSheetAt(i);
                String sheetName = sheet.getSheetName();

                // 添加工作表标题
                text.append("\n\n## 工作表: ").append(sheetName).append("\n\n");

                // 提取表格数据并转换为 Markdown
                String tableMarkdown = extractSheetAsMarkdown(sheet);
                text.append(tableMarkdown).append("\n");
            }

            workbook.close();

            String content = text.toString();
            log.info("✅ Excel 处理完成: {} ({} 个工作表)", documentId, sheetCount);

            return ProcessedDocument.builder()
                    .documentId(documentId)
                    .documentType(DocumentType.EXCEL)
                    .text(content)
                    .characterCount(content.length())
                    .metadata(metadata)
                    .success(true)
                    .build();

        } catch (Exception e) {
            log.error("❌ Excel 处理失败: {}", documentId, e);
            throw new ProcessorException("Excel 处理失败: " + e.getMessage(), e);
        }
    }

    /**
     * 将工作表转换为 Markdown 表格
     */
    private String extractSheetAsMarkdown(Sheet sheet) {
        StringBuilder markdown = new StringBuilder();

        int firstRowNum = sheet.getFirstRowNum();
        int lastRowNum = Math.min(sheet.getLastRowNum(), firstRowNum + MAX_ROWS_PER_SHEET - 1);

        if (lastRowNum < firstRowNum) {
            return "_（工作表为空）_\n";
        }

        // 计算最大列数
        int maxCols = 0;
        for (int rowIdx = firstRowNum; rowIdx <= lastRowNum; rowIdx++) {
            Row row = sheet.getRow(rowIdx);
            if (row != null) {
                maxCols = Math.max(maxCols, row.getLastCellNum());
            }
        }
        maxCols = Math.min(maxCols, MAX_COLS_PER_SHEET);

        if (maxCols == 0) {
            return "_（工作表为空）_\n";
        }

        // 收集表格数据
        List<List<String>> tableData = new ArrayList<>();
        for (int rowIdx = firstRowNum; rowIdx <= lastRowNum; rowIdx++) {
            Row row = sheet.getRow(rowIdx);
            List<String> rowData = new ArrayList<>();

            for (int colIdx = 0; colIdx < maxCols; colIdx++) {
                String cellValue = "";
                if (row != null) {
                    Cell cell = row.getCell(colIdx);
                    cellValue = getCellValueAsString(cell);
                }
                rowData.add(cellValue);
            }

            // 只添加非空行
            if (rowData.stream().anyMatch(v -> !v.trim().isEmpty())) {
                tableData.add(rowData);
            }
        }

        if (!tableData.isEmpty()) {
            return convertToMarkdownTable(tableData) + "\n";
        }

        return "_（工作表无有效数据）_\n";
    }

    /**
     * 获取单元格值（完整类型处理）
     */
    private String getCellValueAsString(Cell cell) {
        if (cell == null) {
            return "";
        }

        try {
            switch (cell.getCellType()) {
                case STRING:
                    return cell.getStringCellValue().trim();

                case NUMERIC:
                    // 处理日期
                    if (DateUtil.isCellDateFormatted(cell)) {
                        return cell.getDateCellValue().toString();
                    }
                    // 处理数字
                    double numValue = cell.getNumericCellValue();
                    if (numValue == (long) numValue) {
                        return String.valueOf((long) numValue);
                    } else {
                        DecimalFormat df = new DecimalFormat("#.##");
                        return df.format(numValue);
                    }

                case BOOLEAN:
                    return String.valueOf(cell.getBooleanCellValue());

                case FORMULA:
                    // 尝试获取公式计算结果
                    try {
                        return getCellFormulaValue(cell);
                    } catch (Exception e) {
                        return cell.getCellFormula();
                    }

                case BLANK:
                    return "";

                default:
                    return "";
            }
        } catch (Exception e) {
            log.warn("获取单元格值失败: {}", e.getMessage());
            return "";
        }
    }

    /**
     * 获取公式单元格的计算值
     */
    private String getCellFormulaValue(Cell cell) {
        CellType cachedType = cell.getCachedFormulaResultType();
        switch (cachedType) {
            case STRING:
                return cell.getStringCellValue();
            case NUMERIC:
                double numValue = cell.getNumericCellValue();
                if (numValue == (long) numValue) {
                    return String.valueOf((long) numValue);
                } else {
                    DecimalFormat df = new DecimalFormat("#.##");
                    return df.format(numValue);
                }
            case BOOLEAN:
                return String.valueOf(cell.getBooleanCellValue());
            default:
                return "";
        }
    }

    /**
     * 转换为 Markdown 表格
     */
    private String convertToMarkdownTable(List<List<String>> tableData) {
        if (tableData.isEmpty()) {
            return "";
        }

        StringBuilder md = new StringBuilder();

        // 表头（第一行）
        List<String> header = tableData.get(0);
        md.append("| ");
        for (String cell : header) {
            md.append(escapeMarkdown(cell)).append(" | ");
        }
        md.append("\n");

        // 分隔线
        md.append("|");
        for (int i = 0; i < header.size(); i++) {
            md.append(" --- |");
        }
        md.append("\n");

        // 数据行
        for (int i = 1; i < tableData.size(); i++) {
            List<String> row = tableData.get(i);
            md.append("| ");
            for (String cell : row) {
                md.append(escapeMarkdown(cell)).append(" | ");
            }
            md.append("\n");
        }

        return md.toString();
    }

    /**
     * 转义 Markdown 特殊字符
     */
    private String escapeMarkdown(String text) {
        if (text == null || text.isEmpty()) {
            return "";
        }
        return text.replace("|", "\\|")
                   .replace("\n", "<br>")
                   .replace("\r", "");
    }

    @Override
    public List<DocumentType> getSupportedTypes() {
        return Arrays.asList(DocumentType.EXCEL);
    }

    @Override
    public boolean supports(DocumentType type) {
        return type == DocumentType.EXCEL;
    }

    @Override
    public boolean supportsExtension(String extension) {
        return ".xls".equalsIgnoreCase(extension) ||
               ".xlsx".equalsIgnoreCase(extension);
    }

    /**
     * 提取工作表中的图片
     */
    private List<ExtractedImage> extractImagesFromSheet(Sheet sheet, int sheetIndex) {
        List<ExtractedImage> images = new ArrayList<>();

        try {
            if (sheet instanceof XSSFSheet) {
                XSSFDrawing drawing = ((XSSFSheet) sheet).getDrawingPatriarch();
                if (drawing != null) {
                    for (XSSFShape shape : drawing.getShapes()) {
                        if (shape instanceof XSSFPicture) {
                            ExtractedImage image = extractXSSFPicture((XSSFPicture) shape, sheet, sheetIndex);
                            if (image != null) {
                                images.add(image);
                            }
                        }
                    }
                }
            } else if (sheet instanceof HSSFSheet) {
                HSSFPatriarch patriarch = ((HSSFSheet) sheet).getDrawingPatriarch();
                if (patriarch != null) {
                    for (HSSFShape shape : patriarch.getChildren()) {
                        if (shape instanceof HSSFPicture) {
                            ExtractedImage image = extractHSSFPicture((HSSFPicture) shape, sheet, sheetIndex);
                            if (image != null) {
                                images.add(image);
                            }
                        }
                    }
                }
            }
        } catch (Exception e) {
            log.warn("提取工作表图片失败: {}", sheet.getSheetName(), e);
        }

        return images;
    }

    private ExtractedImage extractXSSFPicture(XSSFPicture picture, Sheet sheet, int sheetIndex) {
        try {
            XSSFPictureData pictureData = picture.getPictureData();
            XSSFClientAnchor anchor = picture.getClientAnchor();

            Map<String, Object> metadata = new HashMap<>();
            metadata.put("sheetName", sheet.getSheetName());
            metadata.put("sheetIndex", sheetIndex);
            metadata.put("location", String.format("第%d行, 第%d列",
                    anchor.getRow1() + 1, anchor.getCol1() + 1));

            return ExtractedImage.builder()
                    .imageId(UUID.randomUUID().toString())
                    .data(pictureData.getData())
                    .format(pictureData.suggestFileExtension())
                    .pageNumber(sheetIndex)
                    .position(ExtractedImage.ImagePosition.builder()
                            .row((int) anchor.getRow1())
                            .column((int) anchor.getCol1())
                            .description(String.format("第%d行, 第%d列",
                                    anchor.getRow1() + 1, anchor.getCol1() + 1))
                            .build())
                    .metadata(metadata)
                    .createdAt(System.currentTimeMillis())
                    .build();
        } catch (Exception e) {
            log.warn("提取 Excel 图片失败", e);
            return null;
        }
    }

    private ExtractedImage extractHSSFPicture(HSSFPicture picture, Sheet sheet, int sheetIndex) {
        try {
            HSSFPictureData pictureData = picture.getPictureData();
            HSSFClientAnchor anchor = picture.getClientAnchor();

            Map<String, Object> metadata = new HashMap<>();
            metadata.put("sheetName", sheet.getSheetName());
            metadata.put("sheetIndex", sheetIndex);
            metadata.put("location", String.format("第%d行, 第%d列",
                    anchor.getRow1() + 1, anchor.getCol1() + 1));

            return ExtractedImage.builder()
                    .imageId(UUID.randomUUID().toString())
                    .data(pictureData.getData())
                    .format(pictureData.suggestFileExtension())
                    .pageNumber(sheetIndex)
                    .position(ExtractedImage.ImagePosition.builder()
                            .row((int) anchor.getRow1())
                            .column((int) anchor.getCol1())
                            .description(String.format("第%d行, 第%d列",
                                    anchor.getRow1() + 1, anchor.getCol1() + 1))
                            .build())
                    .metadata(metadata)
                    .createdAt(System.currentTimeMillis())
                    .build();
        } catch (Exception e) {
            log.warn("提取 Excel 图片失败", e);
            return null;
        }
    }
}

