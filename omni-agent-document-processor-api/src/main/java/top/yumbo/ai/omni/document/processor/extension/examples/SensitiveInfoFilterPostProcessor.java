package top.yumbo.ai.omni.document.processor.extension.examples;

import lombok.extern.slf4j.Slf4j;
import org.springframework.core.annotation.Order;
import org.springframework.stereotype.Component;
import top.yumbo.ai.omni.document.processor.DocumentProcessor.ProcessingContext;
import top.yumbo.ai.omni.document.processor.DocumentProcessor.ProcessingResult;
import top.yumbo.ai.omni.document.processor.extension.PostProcessor;

import java.util.regex.Matcher;
import java.util.regex.Pattern;

/**
 * 敏感信息过滤后置处理器示例
 * (Sensitive Information Filter Post-Processor Example)
 *
 * <p>
 * 这是一个示例，展示如何使用后置处理器来过滤敏感信息。
 * 用户可以参考这个示例创建自己的后置处理器。
 * </p>
 *
 * <p>功能：</p>
 * <ul>
 *   <li>过滤手机号（替换为 ***-****-****）</li>
 *   <li>过滤邮箱（替换为 ***@***.***）</li>
 *   <li>过滤身份证号（替换为 ******）</li>
 * </ul>
 *
 * @author OmniAgent Team
 * @since 3.0.0
 */
@Slf4j
@Component
@Order(10)  // 较低优先级，在其他处理器之后执行
public class SensitiveInfoFilterPostProcessor implements PostProcessor {

    // 手机号正则
    private static final Pattern PHONE_PATTERN = Pattern.compile("1[3-9]\\d{9}");

    // 邮箱正则
    private static final Pattern EMAIL_PATTERN = Pattern.compile("[a-zA-Z0-9._%+-]+@[a-zA-Z0-9.-]+\\.[a-zA-Z]{2,}");

    // 身份证号正则
    private static final Pattern ID_CARD_PATTERN = Pattern.compile("\\d{17}[\\dXx]");

    @Override
    public String getName() {
        return "SensitiveInfoFilterPostProcessor";
    }

    @Override
    public int getOrder() {
        return 10;
    }

    @Override
    public ProcessingResult postProcess(ProcessingContext context, ProcessingResult result) throws Exception {
        log.debug("📋 [SensitiveInfoFilter] 开始过滤敏感信息");

        String content = result.getContent();
        if (content == null || content.isEmpty()) {
            return result;
        }

        // 过滤手机号
        Matcher phoneMatcher = PHONE_PATTERN.matcher(content);
        content = phoneMatcher.replaceAll("***-****-****");

        // 过滤邮箱
        Matcher emailMatcher = EMAIL_PATTERN.matcher(content);
        content = emailMatcher.replaceAll("***@***.***");

        // 过滤身份证号
        Matcher idCardMatcher = ID_CARD_PATTERN.matcher(content);
        content = idCardMatcher.replaceAll("******************");

        result.setContent(content);

        log.debug("✅ [SensitiveInfoFilter] 敏感信息过滤完成");
        return result;
    }
}

