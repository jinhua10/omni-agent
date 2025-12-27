package top.yumbo.ai.omni.web.controller;

import lombok.Data;
import lombok.RequiredArgsConstructor;
import lombok.extern.slf4j.Slf4j;
import org.springframework.web.bind.annotation.*;

import java.time.LocalDateTime;
import java.util.*;
import java.util.concurrent.ConcurrentHashMap;
import java.util.concurrent.atomic.AtomicLong;
import java.util.stream.Collectors;

/**
 * 愿望控制器
 * (Wish Controller)
 *
 * <p>处理愿望相关的API请求，包括愿望列表、提交、投票、排行榜等</p>
 *
 * @author OmniAgent Team
 * @since 1.0.0
 */
@Slf4j
@RestController
@RequestMapping("/api/wishes")
@RequiredArgsConstructor
public class WishController {

    // 内存存储（用于演示，生产环境应使用数据库）
    private static final Map<Long, WishDTO> wishStore = new ConcurrentHashMap<>();
    private static final Map<Long, Set<String>> voteStore = new ConcurrentHashMap<>();
    private static final AtomicLong idGenerator = new AtomicLong(1);

    static {
        // 初始化一些示例数据
        initSampleData();
    }

    /**
     * 获取愿望列表
     * GET /api/wishes
     */
    @GetMapping
    public PageResponse<WishDTO> getWishes(
            @RequestParam(required = false) String status,
            @RequestParam(required = false) String category,
            @RequestParam(required = false) String sortBy,
            @RequestParam(required = false) String keyword,
            @RequestParam(defaultValue = "0") int page,
            @RequestParam(defaultValue = "20") int size) {

        log.info("获取愿望列表: status={}, category={}, sortBy={}, keyword={}, page={}, size={}",
                status, category, sortBy, keyword, page, size);

        List<WishDTO> filteredWishes = wishStore.values().stream()
                .filter(wish -> status == null || wish.getStatus().equals(status))
                .filter(wish -> category == null || wish.getCategory().equals(category))
                .filter(wish -> keyword == null ||
                        wish.getTitle().toLowerCase().contains(keyword.toLowerCase()) ||
                        wish.getDescription().toLowerCase().contains(keyword.toLowerCase()))
                .sorted(getComparator(sortBy))
                .collect(Collectors.toList());

        // 分页
        int start = page * size;
        int end = Math.min(start + size, filteredWishes.size());
        List<WishDTO> pageContent = start < filteredWishes.size()
                ? filteredWishes.subList(start, end)
                : new ArrayList<>();

        // 创建分页响应
        PageResponse<WishDTO> result = new PageResponse<>();
        result.setContent(pageContent);
        result.setTotalElements(filteredWishes.size());
        result.setTotalPages((int) Math.ceil((double) filteredWishes.size() / size));
        result.setNumber(page);
        result.setSize(size);
        result.setFirst(page == 0);
        result.setLast(page >= result.getTotalPages() - 1);

        log.info("返回愿望列表: total={}, page={}, size={}", result.getTotalElements(), page, size);
        return result;
    }

    /**
     * 获取愿望详情
     * GET /api/wishes/{id}
     */
    @GetMapping("/{id}")
    public WishDTO getWishDetail(@PathVariable Long id) {
        log.info("获取愿望详情: id={}", id);

        WishDTO wish = wishStore.get(id);
        if (wish == null) {
            throw new RuntimeException("愿望不存在: " + id);
        }

        log.info("返回愿望详情: id={}, title={}", id, wish.getTitle());
        return wish;
    }

    /**
     * 提交新愿望
     * POST /api/wishes
     */
    @PostMapping
    public WishDTO submitWish(@RequestBody SubmitWishRequest request) {
        log.info("提交新愿望: title={}, category={}", request.getTitle(), request.getCategory());

        Long id = idGenerator.getAndIncrement();
        WishDTO wish = new WishDTO();
        wish.setId(id);
        wish.setTitle(request.getTitle());
        wish.setDescription(request.getDescription());
        wish.setCategory(request.getCategory());
        wish.setStatus("pending");
        wish.setVotes(0);
        wish.setCommentCount(0);
        wish.setCreatedAt(LocalDateTime.now());
        wish.setUpdatedAt(LocalDateTime.now());
        wish.setUserId(request.getUserId() != null ? request.getUserId() : "anonymous");
        wish.setUserName(request.getUserName() != null ? request.getUserName() : "匿名用户");
        wish.setHasVoted(false);

        wishStore.put(id, wish);
        voteStore.put(id, new HashSet<>());

        log.info("愿望提交成功: id={}", id);
        return wish;
    }

    /**
     * 投票
     * POST /api/wishes/{id}/vote
     */
    @PostMapping("/{id}/vote")
    public Map<String, Object> voteWish(
            @PathVariable Long id,
            @RequestBody VoteRequest request) {

        log.info("投票: id={}, voteType={}, userId={}", id, request.getVoteType(), request.getUserId());

        WishDTO wish = wishStore.get(id);
        if (wish == null) {
            throw new RuntimeException("愿望不存在: " + id);
        }

        String userId = request.getUserId() != null ? request.getUserId() : "anonymous";
        Set<String> voters = voteStore.get(id);

        Map<String, Object> result = new HashMap<>();

        if ("up".equals(request.getVoteType())) {
            // 点赞
            if (voters.add(userId)) {
                wish.setVotes(wish.getVotes() + 1);
                wish.setHasVoted(true);
                result.put("success", true);
                result.put("message", "投票成功");
                result.put("votes", wish.getVotes());
                log.info("投票成功: id={}, votes={}", id, wish.getVotes());
            } else {
                result.put("success", false);
                result.put("message", "您已经投过票了");
                result.put("votes", wish.getVotes());
                log.info("重复投票: id={}, userId={}", id, userId);
            }
        } else if ("down".equals(request.getVoteType())) {
            // 取消点赞
            if (voters.remove(userId)) {
                wish.setVotes(Math.max(wish.getVotes() - 1, 0));
                wish.setHasVoted(false);
                result.put("success", true);
                result.put("message", "取消投票成功");
                result.put("votes", wish.getVotes());
                log.info("取消投票成功: id={}, votes={}", id, wish.getVotes());
            } else {
                result.put("success", false);
                result.put("message", "您还未投票");
                result.put("votes", wish.getVotes());
                log.info("取消投票失败（未投票）: id={}, userId={}", id, userId);
            }
        }

        return result;
    }

    /**
     * 获取排行榜
     * GET /api/wishes/ranking
     */
    @GetMapping("/ranking")
    public List<WishDTO> getRanking(@RequestParam(defaultValue = "10") int limit) {
        log.info("获取排行榜: limit={}", limit);

        List<WishDTO> ranking = wishStore.values().stream()
                .sorted(Comparator.comparing(WishDTO::getVotes).reversed())
                .limit(limit)
                .collect(Collectors.toList());

        log.info("返回排行榜: size={}", ranking.size());
        return ranking;
    }

    // ========== 辅助方法 ==========

    /**
     * 根据排序方式获取比较器
     */
    private Comparator<WishDTO> getComparator(String sortBy) {
        if ("hottest".equals(sortBy)) {
            return Comparator.comparing(WishDTO::getVotes).reversed();
        } else if ("most_voted".equals(sortBy)) {
            return Comparator.comparing(WishDTO::getVotes).reversed();
        } else {
            // 默认按创建时间倒序（最新）
            return Comparator.comparing(WishDTO::getCreatedAt).reversed();
        }
    }

    /**
     * 初始化示例数据
     */
    private static void initSampleData() {
        // 示例愿望 1
        WishDTO wish1 = new WishDTO();
        wish1.setId(1L);
        wish1.setTitle("支持更多AI模型");
        wish1.setDescription("希望能够支持更多的AI模型，例如Claude、Gemini等");
        wish1.setCategory("feature");
        wish1.setStatus("pending");
        wish1.setVotes(15);
        wish1.setCommentCount(3);
        wish1.setCreatedAt(LocalDateTime.now().minusDays(5));
        wish1.setUpdatedAt(LocalDateTime.now().minusDays(5));
        wish1.setUserId("user1");
        wish1.setUserName("张三");
        wish1.setHasVoted(false);
        wishStore.put(1L, wish1);
        voteStore.put(1L, new HashSet<>());

        // 示例愿望 2
        WishDTO wish2 = new WishDTO();
        wish2.setId(2L);
        wish2.setTitle("优化响应速度");
        wish2.setDescription("希望能够优化AI响应速度，减少等待时间");
        wish2.setCategory("improvement");
        wish2.setStatus("pending");
        wish2.setVotes(23);
        wish2.setCommentCount(5);
        wish2.setCreatedAt(LocalDateTime.now().minusDays(3));
        wish2.setUpdatedAt(LocalDateTime.now().minusDays(3));
        wish2.setUserId("user2");
        wish2.setUserName("李四");
        wish2.setHasVoted(false);
        wishStore.put(2L, wish2);
        voteStore.put(2L, new HashSet<>());

        // 示例愿望 3
        WishDTO wish3 = new WishDTO();
        wish3.setId(3L);
        wish3.setTitle("添加语音输入功能");
        wish3.setDescription("希望能够通过语音输入进行提问");
        wish3.setCategory("feature");
        wish3.setStatus("pending");
        wish3.setVotes(8);
        wish3.setCommentCount(2);
        wish3.setCreatedAt(LocalDateTime.now().minusDays(1));
        wish3.setUpdatedAt(LocalDateTime.now().minusDays(1));
        wish3.setUserId("user3");
        wish3.setUserName("王五");
        wish3.setHasVoted(false);
        wishStore.put(3L, wish3);
        voteStore.put(3L, new HashSet<>());

        idGenerator.set(4L);

        log.info("初始化示例愿望数据完成: count={}", wishStore.size());
    }

    // ========== 请求/响应类 ==========

    @Data
    public static class SubmitWishRequest {
        private String title;
        private String description;
        private String category;
        private String userId;
        private String userName;
    }

    @Data
    public static class VoteRequest {
        private String voteType; // up/down
        private String userId;
    }

    @Data
    public static class WishDTO {
        private Long id;
        private String title;
        private String description;
        private String category;
        private String status;
        private Integer votes;
        private Integer commentCount;
        private LocalDateTime createdAt;
        private LocalDateTime updatedAt;
        private String userId;
        private String userName;
        private Boolean hasVoted;
    }

    @Data
    public static class PageResponse<T> {
        private List<T> content;
        private int totalElements;
        private int totalPages;
        private int number;
        private int size;
        private boolean first;
        private boolean last;
    }
}



