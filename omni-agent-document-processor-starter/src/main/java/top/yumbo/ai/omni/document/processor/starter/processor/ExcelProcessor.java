package top.yumbo.ai.omni.document.processor.starter.processor;

import lombok.extern.slf4j.Slf4j;
import org.apache.poi.ss.usermodel.*;
import top.yumbo.ai.omni.document.processor.*;
import top.yumbo.ai.omni.document.processor.starter.config.DocumentProcessorProperties;

import java.io.InputStream;
import java.util.Arrays;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

/**
 * Excel 文档处理器
 *
 * <p>支持 .xls 和 .xlsx 格式</p>
 *
 * @author OmniAgent Team
 * @since 1.0.0
 */
@Slf4j
public class ExcelProcessor implements DocumentProcessor {

    private final DocumentProcessorProperties properties;

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
            int totalRows = 0;
            int maxRows = properties.getExcel().getMaxRows();

            for (int i = 0; i < sheetCount; i++) {
                Sheet sheet = workbook.getSheetAt(i);
                String sheetName = sheet.getSheetName();

                text.append("## ").append(sheetName).append("\n\n");

                for (Row row : sheet) {
                    if (totalRows >= maxRows) {
                        text.append("\n... (已达到最大行数限制: ").append(maxRows).append(")\n");
                        break;
                    }

                    StringBuilder rowText = new StringBuilder();
                    for (Cell cell : row) {
                        String cellValue = getCellValue(cell);
                        if (cellValue != null && !cellValue.isEmpty()) {
                            if (rowText.length() > 0) {
                                rowText.append("\t");
                            }
                            rowText.append(cellValue);
                        }
                    }

                    if (rowText.length() > 0) {
                        text.append(rowText).append("\n");
                    }
                    totalRows++;
                }

                text.append("\n");
            }

            workbook.close();

            Map<String, Object> metadata = new HashMap<>();
            metadata.put("sheetCount", sheetCount);
            metadata.put("totalRows", totalRows);

            String content = text.toString();
            log.info("✅ Excel 处理完成: {} ({} 个工作表, {} 行)",
                documentId, sheetCount, totalRows);

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

    private String getCellValue(Cell cell) {
        if (cell == null) {
            return "";
        }

        switch (cell.getCellType()) {
            case STRING:
                return cell.getStringCellValue();
            case NUMERIC:
                if (DateUtil.isCellDateFormatted(cell)) {
                    return cell.getDateCellValue().toString();
                }
                return String.valueOf(cell.getNumericCellValue());
            case BOOLEAN:
                return String.valueOf(cell.getBooleanCellValue());
            case FORMULA:
                try {
                    return String.valueOf(cell.getNumericCellValue());
                } catch (Exception e) {
                    return cell.getStringCellValue();
                }
            default:
                return "";
        }
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
}

