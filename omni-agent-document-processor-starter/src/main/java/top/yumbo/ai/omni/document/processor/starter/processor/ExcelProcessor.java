package top.yumbo.ai.omni.document.processor.starter.processor;

import lombok.extern.slf4j.Slf4j;
import org.apache.poi.hssf.usermodel.*;
import org.apache.poi.ss.usermodel.*;
import org.apache.poi.xssf.usermodel.*;
import org.springframework.boot.autoconfigure.condition.ConditionalOnProperty;
import org.springframework.stereotype.Component;
import top.yumbo.ai.omni.document.processor.AbstractDocumentProcessor;

import java.io.ByteArrayInputStream;
import java.io.FileInputStream;
import java.io.InputStream;
import java.text.DecimalFormat;
import java.util.*;

/**
 * Excel 文档处理器
 * (Excel Document Processor)
 *
 * <p>处理策略：</p>
 * <ul>
 *   <li>提取所有工作表的表格数据（Markdown 格式）</li>
 *   <li>提取内嵌图片的位置信息</li>
 *   <li>使用 Vision LLM 分析图片内容</li>
 *   <li>将图片描述嵌入到表格对应位置</li>
 * </ul>
 *
 * <p>配置说明：</p>
 * <ul>
 *   <li>默认启用（matchIfMissing = true），无需配置</li>
 *   <li>可通过 omni-agent.excel.enabled=false 禁用</li>
 *   <li>未来可能添加更多配置项（如 max-rows、max-sheets 等）</li>
 * </ul>
 *
 * @author OmniAgent Team
 * @since 3.0.0
 */
@Slf4j
@Component
@ConditionalOnProperty(
    prefix = "omni-agent.excel",
    name = "enabled",
    havingValue = "true",
    matchIfMissing = true  // 默认启用，无需配置
)
public class ExcelProcessor extends AbstractDocumentProcessor {

    private static final Set<String> SUPPORTED_EXTENSIONS = Set.of("xls", "xlsx");
    private static final int MAX_ROWS_PER_SHEET = 1000;
    private static final int MAX_COLS_PER_SHEET = 50;

    @Override
    public boolean supports(String fileExtension) {
        return SUPPORTED_EXTENSIONS.contains(fileExtension.toLowerCase());
    }

    @Override
    public String getName() {
        return "ExcelProcessor";
    }

    @Override
    public int getPriority() {
        return 30; // 高优先级
    }

    @Override
    protected ExtractedContent extractContent(ProcessingContext context) throws Exception {
        String ext = context.getFileExtension().toLowerCase();

        InputStream inputStream;
        if (context.getFileBytes() != null) {
            inputStream = new ByteArrayInputStream(context.getFileBytes());
        } else {
            inputStream = new FileInputStream(context.getFilePath());
        }

        Workbook workbook;
        if ("xlsx".equals(ext)) {
            workbook = new XSSFWorkbook(inputStream);
        } else {
            workbook = new HSSFWorkbook(inputStream);
        }

        ExtractedContent content = new ExtractedContent();
        content.getMetadata().put("totalSheets", workbook.getNumberOfSheets());
        content.getMetadata().put("format", ext);

        int position = 0;

        for (int i = 0; i < workbook.getNumberOfSheets(); i++) {
            Sheet sheet = workbook.getSheetAt(i);
            String sheetName = sheet.getSheetName();

            // 添加工作表标题
            String sheetHeader = "\n\n## 工作表: " + sheetName + "\n\n";
            content.addTextBlock(sheetHeader, position++);

            // 提取表格数据
            String tableText = extractSheetAsMarkdown(sheet);
            content.addTextBlock(tableText, position++);

            // 提取图片
            List<ExtractedImage> images = extractImagesFromSheet(sheet, i + 1, ext, workbook);
            if (!images.isEmpty()) {
                content.addImageBlock(images, position++);
            }
        }

        workbook.close();
        return content;
    }

    /**
     * 提取工作表中的图片
     */
    private List<ExtractedImage> extractImagesFromSheet(Sheet sheet, int sheetIndex, String ext, Workbook workbook) {
        List<ExtractedImage> images = new ArrayList<>();

        try {
            if ("xlsx".equals(ext)) {
                XSSFDrawing drawing = ((XSSFSheet) sheet).getDrawingPatriarch();
                if (drawing != null) {
                    for (XSSFShape shape : drawing.getShapes()) {
                        if (shape instanceof XSSFPicture picture) {
                            ExtractedImage image = extractXSSFPicture(picture, sheet, sheetIndex);
                            if (image != null) {
                                images.add(image);
                            }
                        }
                    }
                }
            } else {
                HSSFPatriarch patriarch = ((HSSFSheet) sheet).getDrawingPatriarch();
                if (patriarch != null) {
                    for (HSSFShape shape : patriarch.getChildren()) {
                        if (shape instanceof HSSFPicture picture) {
                            ExtractedImage image = extractHSSFPicture(picture, sheet, sheetIndex);
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
            metadata.put("imageIndex", 0);

            return ExtractedImage.builder()
                    .data(pictureData.getData())
                    .format(pictureData.suggestFileExtension())
                    .pageNumber(sheetIndex)
                    .position(new VisionLLMDocumentProcessor.ImagePosition(anchor.getCol1(), anchor.getRow1(), 0, 0))
                    .metadata(metadata)
                    .build();
        } catch (Exception e) {
            log.warn("提取图片失败", e);
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
            metadata.put("imageIndex", 0);

            return ExtractedImage.builder()
                    .data(pictureData.getData())
                    .format(pictureData.suggestFileExtension())
                    .pageNumber(sheetIndex)
                    .position(new VisionLLMDocumentProcessor.ImagePosition(anchor.getCol1(), anchor.getRow1(), 0, 0))
                    .metadata(metadata)
                    .build();
        } catch (Exception e) {
            log.warn("提取图片失败", e);
            return null;
        }
    }

    private String extractSheetAsMarkdown(Sheet sheet) {
        // ... 保留原有的 Markdown 转换逻辑 ...
        StringBuilder markdown = new StringBuilder();

        int firstRowNum = sheet.getFirstRowNum();
        int lastRowNum = Math.min(sheet.getLastRowNum(), firstRowNum + MAX_ROWS_PER_SHEET - 1);

        if (lastRowNum < firstRowNum) {
            return "_（工作表为空）_\n";
        }

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

            if (rowData.stream().anyMatch(v -> !v.trim().isEmpty())) {
                tableData.add(rowData);
            }
        }

        if (!tableData.isEmpty()) {
            return convertToMarkdownTable(tableData) + "\n";
        }

        return "_（工作表无有效数据）_\n";
    }

    // ... 保留原有的辅助方法：getCellValueAsString, convertToMarkdownTable 等 ...

    private String getCellValueAsString(Cell cell) {
        if (cell == null) {
            return "";
        }

        try {
            switch (cell.getCellType()) {
                case STRING:
                    return cell.getStringCellValue().trim();
                case NUMERIC:
                    if (DateUtil.isCellDateFormatted(cell)) {
                        return cell.getDateCellValue().toString();
                    } else {
                        double numValue = cell.getNumericCellValue();
                        if (numValue == (long) numValue) {
                            return String.valueOf((long) numValue);
                        } else {
                            DecimalFormat df = new DecimalFormat("#.##");
                            return df.format(numValue);
                        }
                    }
                case BOOLEAN:
                    return String.valueOf(cell.getBooleanCellValue());
                case FORMULA:
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

    private String convertToMarkdownTable(List<List<String>> tableData) {
        if (tableData.isEmpty()) {
            return "";
        }

        StringBuilder md = new StringBuilder();

        // 表头
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

    private String escapeMarkdown(String text) {
        if (text == null || text.isEmpty()) {
            return "";
        }
        return text.replace("|", "\\|")
                   .replace("\n", "<br>")
                   .replace("\r", "");
    }

    @Override
    public ValidationResult validate(ProcessingContext context) {
        if (context.getFileSize() > 100 * 1024 * 1024) {
            return ValidationResult.builder()
                    .valid(false)
                    .message("Excel 文件过大（超过100MB）")
                    .build();
        }

        return ValidationResult.builder()
                .valid(true)
                .message("验证通过")
                .build();
    }
}


