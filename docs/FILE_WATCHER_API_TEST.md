# 文件监听 API 测试脚本

## 1. 获取当前配置
```bash
curl http://localhost:8080/api/file-watcher/config
```

## 2. 禁用自动索引（手动模式）
```bash
curl -X PUT http://localhost:8080/api/file-watcher/config \
  -H "Content-Type: application/json" \
  -d '{"autoIndex": false}'
```

## 3. 启用自动索引
```bash
curl -X PUT http://localhost:8080/api/file-watcher/config \
  -H "Content-Type: application/json" \
  -d '{"autoIndex": true}'
```

## 4. 查看未处理的文件变化
```bash
curl http://localhost:8080/api/file-watcher/changes/unprocessed
```

## 5. 查看所有文件变化
```bash
curl http://localhost:8080/api/file-watcher/changes
```

## 6. 手动处理单个变化（替换 {recordId}）
```bash
curl -X POST http://localhost:8080/api/file-watcher/changes/{recordId}/process
```

## 7. 批量处理所有未处理的变化
```bash
curl -X POST http://localhost:8080/api/file-watcher/changes/process-all
```

## 8. 清除已处理的记录
```bash
curl -X DELETE http://localhost:8080/api/file-watcher/changes/processed
```

## 测试场景

### 场景1：手动模式测试

1. 设置为手动模式
```bash
curl -X PUT http://localhost:8080/api/file-watcher/config \
  -H "Content-Type: application/json" \
  -d '{"autoIndex": false}'
```

2. 在 `data/documents/` 目录创建新文件
```bash
echo "test content" > data/documents/test-$(date +%s).txt
```

3. 查看未处理的变化
```bash
curl http://localhost:8080/api/file-watcher/changes/unprocessed
```

4. 手动处理所有变化
```bash
curl -X POST http://localhost:8080/api/file-watcher/changes/process-all
```

### 场景2：自动模式测试

1. 设置为自动模式
```bash
curl -X PUT http://localhost:8080/api/file-watcher/config \
  -H "Content-Type: application/json" \
  -d '{"autoIndex": true}'
```

2. 创建新文件（会自动索引）
```bash
echo "auto index test" > data/documents/auto-test.txt
```

3. 查看变化（应该已经被处理）
```bash
curl http://localhost:8080/api/file-watcher/changes
```

## PowerShell 版本

### 获取配置
```powershell
Invoke-RestMethod -Uri "http://localhost:8080/api/file-watcher/config" -Method Get
```

### 更新配置
```powershell
$body = @{
    autoIndex = $false
} | ConvertTo-Json

Invoke-RestMethod -Uri "http://localhost:8080/api/file-watcher/config" `
    -Method Put `
    -Body $body `
    -ContentType "application/json"
```

### 查看未处理的变化
```powershell
Invoke-RestMethod -Uri "http://localhost:8080/api/file-watcher/changes/unprocessed" -Method Get
```

### 批量处理
```powershell
Invoke-RestMethod -Uri "http://localhost:8080/api/file-watcher/changes/process-all" -Method Post
```

