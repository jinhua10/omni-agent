package top.yumbo.ai.persistence.file;

import com.fasterxml.jackson.databind.ObjectMapper;
import com.fasterxml.jackson.databind.SerializationFeature;
import com.fasterxml.jackson.datatype.jsr310.JavaTimeModule;
import lombok.extern.slf4j.Slf4j;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.stereotype.Component;
import top.yumbo.ai.persistence.api.QuestionClassifierPersistence;
import top.yumbo.ai.persistence.api.model.QuestionTypeConfig;

import jakarta.annotation.PostConstruct;
import java.io.File;
import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.Paths;
import java.nio.file.StandardCopyOption;
import java.time.LocalDateTime;
import java.time.format.DateTimeFormatter;
import java.util.*;
import java.util.concurrent.ConcurrentHashMap;
import java.util.concurrent.locks.ReadWriteLock;
import java.util.concurrent.locks.ReentrantReadWriteLock;

/**
 * File 持久化实现 - 基于 JSON 文件存储
 * (File Persistence Implementation - JSON file based storage)
 *
 * <p>
 * 特点 (Features):
 * - JSON 格式存储，易读易调试
 * - 支持自动备份和恢复
 * - 线程安全（读写锁）
 * - 变更历史记录
 * - 适合小规模数据和单机部署
 * </p>
 *
 * @author OmniAgent Team
 * @since 1.0.0
 */
@Slf4j
@Component
public class FilePersistence implements QuestionClassifierPersistence {

    private final ObjectMapper objectMapper;
    private final ReadWriteLock lock = new ReentrantReadWriteLock();
    
    @Value("${omni.persistence.file.data-dir:./data/persistence}")
    private String dataDir;
    
    @Value("${omni.persistence.file.backup-dir:./data/persistence/backups}")
    private String backupDir;
    
    @Value("${omni.persistence.file.auto-save:true}")
    private boolean autoSave;
    
    private Path dataPath;
    private Path backupPath;
    
    // 内存缓存
    private final Map<String, QuestionTypeConfig> typeConfigs = new ConcurrentHashMap<>();
    private final Map<String, List<String>> keywords = new ConcurrentHashMap<>();
    private final Map<String, List<String>> patterns = new ConcurrentHashMap<>();
    private final List<FileChangeRecord> changeHistory = new ArrayList<>();
    private String version = "1.0.0";
    
    // 文件名常量
    private static final String TYPE_CONFIGS_FILE = "type-configs.json";
    private static final String KEYWORDS_FILE = "keywords.json";
    private static final String PATTERNS_FILE = "patterns.json";
    private static final String METADATA_FILE = "metadata.json";
    private static final String HISTORY_FILE = "change-history.json";

    public FilePersistence() {
        this.objectMapper = new ObjectMapper();
        this.objectMapper.registerModule(new JavaTimeModule());
        this.objectMapper.enable(SerializationFeature.INDENT_OUTPUT);
        this.objectMapper.disable(SerializationFeature.WRITE_DATES_AS_TIMESTAMPS);
    }

    @PostConstruct
    public void init() {
        try {
            // 创建数据目录
            dataPath = Paths.get(dataDir);
            backupPath = Paths.get(backupDir);
            Files.createDirectories(dataPath);
            Files.createDirectories(backupPath);
            
            // 加载已有数据
            loadFromDisk();
            
            log.info("✅ FilePersistence initialized - Data directory: {}", dataPath.toAbsolutePath());
        } catch (IOException e) {
            log.error("❌ Failed to initialize FilePersistence", e);
            throw new RuntimeException("Failed to initialize file persistence", e);
        }
    }

    // ========== 问题类型管理 ==========

    @Override
    public boolean saveQuestionType(QuestionTypeConfig config) {
        if (config == null || config.getId() == null) {
            return false;
        }
        
        lock.writeLock().lock();
        try {
            typeConfigs.put(config.getId(), config);
            
            if (autoSave) {
                saveTypeConfigsToDisk();
            }
            
            recordChange(new FileChangeRecord(
                UUID.randomUUID().toString(),
                config.getId(),
                "CREATE",
                "system",
                System.currentTimeMillis(),
                Map.of("name", config.getName())
            ));
            
            log.debug("📝 Saved question type: {}", config.getId());
            return true;
        } catch (Exception e) {
            log.error("❌ Failed to save question type: {}", config.getId(), e);
            return false;
        } finally {
            lock.writeLock().unlock();
        }
    }

    @Override
    public int saveQuestionTypes(List<QuestionTypeConfig> configs) {
        if (configs == null) {
            return 0;
        }
        
        int count = 0;
        lock.writeLock().lock();
        try {
            for (QuestionTypeConfig config : configs) {
                if (config != null && config.getId() != null) {
                    typeConfigs.put(config.getId(), config);
                    count++;
                }
            }
            
            if (autoSave && count > 0) {
                saveTypeConfigsToDisk();
            }
            
            log.debug("📝 Batch saved {} question types", count);
            return count;
        } catch (Exception e) {
            log.error("❌ Failed to batch save question types", e);
            return count;
        } finally {
            lock.writeLock().unlock();
        }
    }

    @Override
    public Optional<QuestionTypeConfig> getQuestionType(String typeId) {
        lock.readLock().lock();
        try {
            return Optional.ofNullable(typeConfigs.get(typeId));
        } finally {
            lock.readLock().unlock();
        }
    }

    @Override
    public List<QuestionTypeConfig> getAllQuestionTypes() {
        lock.readLock().lock();
        try {
            return new ArrayList<>(typeConfigs.values());
        } finally {
            lock.readLock().unlock();
        }
    }

    @Override
    public boolean updateQuestionType(QuestionTypeConfig config) {
        if (config == null || config.getId() == null) {
            return false;
        }
        
        lock.writeLock().lock();
        try {
            if (!typeConfigs.containsKey(config.getId())) {
                return false;
            }
            
            typeConfigs.put(config.getId(), config);
            
            if (autoSave) {
                saveTypeConfigsToDisk();
            }
            
            recordChange(new FileChangeRecord(
                UUID.randomUUID().toString(),
                config.getId(),
                "UPDATE",
                "system",
                System.currentTimeMillis(),
                Map.of("name", config.getName())
            ));
            
            log.debug("✏️ Updated question type: {}", config.getId());
            return true;
        } catch (Exception e) {
            log.error("❌ Failed to update question type: {}", config.getId(), e);
            return false;
        } finally {
            lock.writeLock().unlock();
        }
    }

    @Override
    public boolean deleteQuestionType(String typeId) {
        lock.writeLock().lock();
        try {
            QuestionTypeConfig removed = typeConfigs.remove(typeId);
            if (removed != null) {
                keywords.remove(typeId);
                patterns.remove(typeId);
                
                if (autoSave) {
                    saveAllToDisk();
                }
                
                recordChange(new FileChangeRecord(
                    UUID.randomUUID().toString(),
                    typeId,
                    "DELETE",
                    "system",
                    System.currentTimeMillis(),
                    Map.of("name", removed.getName())
                ));
                
                log.debug("🗑️ Deleted question type: {}", typeId);
                return true;
            }
            return false;
        } catch (Exception e) {
            log.error("❌ Failed to delete question type: {}", typeId, e);
            return false;
        } finally {
            lock.writeLock().unlock();
        }
    }

    // ========== 关键词管理 ==========

    @Override
    public boolean saveKeywords(String typeId, List<String> keywordList) {
        if (typeId == null || keywordList == null) {
            return false;
        }
        
        lock.writeLock().lock();
        try {
            keywords.put(typeId, new ArrayList<>(keywordList));
            
            if (autoSave) {
                saveKeywordsToDisk();
            }
            
            log.debug("📝 Saved {} keywords for type: {}", keywordList.size(), typeId);
            return true;
        } catch (Exception e) {
            log.error("❌ Failed to save keywords for type: {}", typeId, e);
            return false;
        } finally {
            lock.writeLock().unlock();
        }
    }

    @Override
    public boolean addKeywords(String typeId, List<String> newKeywords) {
        if (typeId == null || newKeywords == null || newKeywords.isEmpty()) {
            return false;
        }
        
        lock.writeLock().lock();
        try {
            List<String> existing = keywords.computeIfAbsent(typeId, k -> new ArrayList<>());
            existing.addAll(newKeywords);
            
            if (autoSave) {
                saveKeywordsToDisk();
            }
            
            log.debug("➕ Added {} keywords to type: {}", newKeywords.size(), typeId);
            return true;
        } catch (Exception e) {
            log.error("❌ Failed to add keywords for type: {}", typeId, e);
            return false;
        } finally {
            lock.writeLock().unlock();
        }
    }

    @Override
    public List<String> getKeywords(String typeId) {
        lock.readLock().lock();
        try {
            return new ArrayList<>(keywords.getOrDefault(typeId, new ArrayList<>()));
        } finally {
            lock.readLock().unlock();
        }
    }

    @Override
    public Map<String, List<String>> getAllKeywords() {
        lock.readLock().lock();
        try {
            return new HashMap<>(keywords);
        } finally {
            lock.readLock().unlock();
        }
    }

    // ========== 模式管理 ==========

    @Override
    public boolean savePatterns(String typeId, List<String> patternList) {
        if (typeId == null || patternList == null) {
            return false;
        }
        
        lock.writeLock().lock();
        try {
            patterns.put(typeId, new ArrayList<>(patternList));
            
            if (autoSave) {
                savePatternsToDisk();
            }
            
            log.debug("📝 Saved {} patterns for type: {}", patternList.size(), typeId);
            return true;
        } catch (Exception e) {
            log.error("❌ Failed to save patterns for type: {}", typeId, e);
            return false;
        } finally {
            lock.writeLock().unlock();
        }
    }

    @Override
    public boolean addPatterns(String typeId, List<String> newPatterns) {
        if (typeId == null || newPatterns == null || newPatterns.isEmpty()) {
            return false;
        }
        
        lock.writeLock().lock();
        try {
            List<String> existing = patterns.computeIfAbsent(typeId, k -> new ArrayList<>());
            existing.addAll(newPatterns);
            
            if (autoSave) {
                savePatternsToDisk();
            }
            
            log.debug("➕ Added {} patterns to type: {}", newPatterns.size(), typeId);
            return true;
        } catch (Exception e) {
            log.error("❌ Failed to add patterns for type: {}", typeId, e);
            return false;
        } finally {
            lock.writeLock().unlock();
        }
    }

    @Override
    public List<String> getPatterns(String typeId) {
        lock.readLock().lock();
        try {
            return new ArrayList<>(patterns.getOrDefault(typeId, new ArrayList<>()));
        } finally {
            lock.readLock().unlock();
        }
    }

    @Override
    public Map<String, List<String>> getAllPatterns() {
        lock.readLock().lock();
        try {
            return new HashMap<>(patterns);
        } finally {
            lock.readLock().unlock();
        }
    }

    // ========== 备份与恢复 ==========

    @Override
    public String createBackup() {
        lock.readLock().lock();
        try {
            String timestamp = LocalDateTime.now().format(DateTimeFormatter.ofPattern("yyyyMMdd_HHmmss"));
            String backupId = "backup_" + timestamp;
            Path backupFolder = backupPath.resolve(backupId);
            Files.createDirectories(backupFolder);
            
            // 复制所有数据文件到备份目录
            Files.copy(
                dataPath.resolve(TYPE_CONFIGS_FILE),
                backupFolder.resolve(TYPE_CONFIGS_FILE),
                StandardCopyOption.REPLACE_EXISTING
            );
            Files.copy(
                dataPath.resolve(KEYWORDS_FILE),
                backupFolder.resolve(KEYWORDS_FILE),
                StandardCopyOption.REPLACE_EXISTING
            );
            Files.copy(
                dataPath.resolve(PATTERNS_FILE),
                backupFolder.resolve(PATTERNS_FILE),
                StandardCopyOption.REPLACE_EXISTING
            );
            Files.copy(
                dataPath.resolve(METADATA_FILE),
                backupFolder.resolve(METADATA_FILE),
                StandardCopyOption.REPLACE_EXISTING
            );
            
            log.info("💾 Created backup: {}", backupId);
            return backupId;
        } catch (IOException e) {
            log.error("❌ Failed to create backup", e);
            return null;
        } finally {
            lock.readLock().unlock();
        }
    }

    @Override
    public boolean restoreFromBackup(String backupId) {
        lock.writeLock().lock();
        try {
            Path backupFolder = backupPath.resolve(backupId);
            if (!Files.exists(backupFolder)) {
                log.error("❌ Backup not found: {}", backupId);
                return false;
            }
            
            // 恢复所有数据文件
            Files.copy(
                backupFolder.resolve(TYPE_CONFIGS_FILE),
                dataPath.resolve(TYPE_CONFIGS_FILE),
                StandardCopyOption.REPLACE_EXISTING
            );
            Files.copy(
                backupFolder.resolve(KEYWORDS_FILE),
                dataPath.resolve(KEYWORDS_FILE),
                StandardCopyOption.REPLACE_EXISTING
            );
            Files.copy(
                backupFolder.resolve(PATTERNS_FILE),
                dataPath.resolve(PATTERNS_FILE),
                StandardCopyOption.REPLACE_EXISTING
            );
            Files.copy(
                backupFolder.resolve(METADATA_FILE),
                dataPath.resolve(METADATA_FILE),
                StandardCopyOption.REPLACE_EXISTING
            );
            
            // 重新加载数据
            loadFromDisk();
            
            log.info("🔄 Restored from backup: {}", backupId);
            return true;
        } catch (IOException e) {
            log.error("❌ Failed to restore from backup: {}", backupId, e);
            return false;
        } finally {
            lock.writeLock().unlock();
        }
    }

    @Override
    public List<String> listBackups() {
        try {
            List<String> backups = new ArrayList<>();
            Files.list(backupPath)
                .filter(Files::isDirectory)
                .map(p -> p.getFileName().toString())
                .sorted(Comparator.reverseOrder())
                .forEach(backups::add);
            return backups;
        } catch (IOException e) {
            log.error("❌ Failed to list backups", e);
            return new ArrayList<>();
        }
    }

    // ========== 版本管理 ==========

    @Override
    public String getVersion() {
        return version;
    }

    @Override
    public boolean saveVersion(String newVersion) {
        lock.writeLock().lock();
        try {
            this.version = newVersion;
            saveMetadata();
            log.debug("📝 Saved version: {}", newVersion);
            return true;
        } catch (Exception e) {
            log.error("❌ Failed to save version", e);
            return false;
        } finally {
            lock.writeLock().unlock();
        }
    }

    // ========== 变更历史 ==========

    @Override
    public List<ChangeRecord> getChangeHistory(int limit) {
        lock.readLock().lock();
        try {
            int size = changeHistory.size();
            int fromIndex = Math.max(0, size - limit);
            return new ArrayList<>(changeHistory.subList(fromIndex, size));
        } finally {
            lock.readLock().unlock();
        }
    }

    @Override
    public boolean recordChange(ChangeRecord change) {
        lock.writeLock().lock();
        try {
            if (change instanceof FileChangeRecord) {
                changeHistory.add((FileChangeRecord) change);
                
                // 保持历史记录不超过1000条
                if (changeHistory.size() > 1000) {
                    changeHistory.remove(0);
                }
                
                if (autoSave) {
                    saveHistoryToDisk();
                }
                
                return true;
            }
            return false;
        } catch (Exception e) {
            log.error("❌ Failed to record change", e);
            return false;
        } finally {
            lock.writeLock().unlock();
        }
    }

    // ========== 私有辅助方法 ==========

    private void loadFromDisk() throws IOException {
        loadTypeConfigs();
        loadKeywords();
        loadPatterns();
        loadMetadata();
        loadHistory();
        log.info("📂 Loaded data from disk");
    }

    private void loadTypeConfigs() throws IOException {
        File file = dataPath.resolve(TYPE_CONFIGS_FILE).toFile();
        if (file.exists()) {
            Map<String, QuestionTypeConfig> loaded = objectMapper.readValue(
                file,
                objectMapper.getTypeFactory().constructMapType(HashMap.class, String.class, QuestionTypeConfig.class)
            );
            typeConfigs.putAll(loaded);
        }
    }

    private void loadKeywords() throws IOException {
        File file = dataPath.resolve(KEYWORDS_FILE).toFile();
        if (file.exists()) {
            Map<String, List<String>> loaded = objectMapper.readValue(
                file,
                objectMapper.getTypeFactory().constructMapType(HashMap.class, String.class, List.class)
            );
            keywords.putAll(loaded);
        }
    }

    private void loadPatterns() throws IOException {
        File file = dataPath.resolve(PATTERNS_FILE).toFile();
        if (file.exists()) {
            Map<String, List<String>> loaded = objectMapper.readValue(
                file,
                objectMapper.getTypeFactory().constructMapType(HashMap.class, String.class, List.class)
            );
            patterns.putAll(loaded);
        }
    }

    @SuppressWarnings("unchecked")
    private void loadMetadata() throws IOException {
        File file = dataPath.resolve(METADATA_FILE).toFile();
        if (file.exists()) {
            Map<String, Object> metadata = objectMapper.readValue(file, HashMap.class);
            this.version = (String) metadata.getOrDefault("version", "1.0.0");
        }
    }

    private void loadHistory() throws IOException {
        File file = dataPath.resolve(HISTORY_FILE).toFile();
        if (file.exists()) {
            List<FileChangeRecord> loaded = objectMapper.readValue(
                file,
                objectMapper.getTypeFactory().constructCollectionType(ArrayList.class, FileChangeRecord.class)
            );
            changeHistory.addAll(loaded);
        }
    }

    private void saveAllToDisk() throws IOException {
        saveTypeConfigsToDisk();
        saveKeywordsToDisk();
        savePatternsToDisk();
        saveMetadata();
        saveHistoryToDisk();
    }

    private void saveTypeConfigsToDisk() throws IOException {
        objectMapper.writeValue(dataPath.resolve(TYPE_CONFIGS_FILE).toFile(), typeConfigs);
    }

    private void saveKeywordsToDisk() throws IOException {
        objectMapper.writeValue(dataPath.resolve(KEYWORDS_FILE).toFile(), keywords);
    }

    private void savePatternsToDisk() throws IOException {
        objectMapper.writeValue(dataPath.resolve(PATTERNS_FILE).toFile(), patterns);
    }

    private void saveMetadata() throws IOException {
        Map<String, Object> metadata = new HashMap<>();
        metadata.put("version", version);
        metadata.put("lastUpdate", LocalDateTime.now().toString());
        objectMapper.writeValue(dataPath.resolve(METADATA_FILE).toFile(), metadata);
    }

    private void saveHistoryToDisk() throws IOException {
        objectMapper.writeValue(dataPath.resolve(HISTORY_FILE).toFile(), changeHistory);
    }

    /**
     * 手动保存所有数据到磁盘
     */
    public void flush() {
        lock.writeLock().lock();
        try {
            saveAllToDisk();
            log.info("💾 Flushed all data to disk");
        } catch (IOException e) {
            log.error("❌ Failed to flush data to disk", e);
        } finally {
            lock.writeLock().unlock();
        }
    }
}
