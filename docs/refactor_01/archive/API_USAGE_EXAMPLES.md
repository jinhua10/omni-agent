# Knowledge Registry API 使用示例

> 知识域管理 REST API 使用指南

---

## 📖 API 端点

**基础路径：** `/api/knowledge-domains`

### 1. 创建知识域

**POST** `/api/knowledge-domains`

**请求体：**
```json
{
  "domainName": "文档知识域",
  "domainType": "DOCUMENT",
  "description": "用于存储文档的知识域",
  "linkedEntityId": null,
  "config": {
    "ragBackend": "lucene",
    "chunkSize": 512
  }
}
```

**响应：**
```json
{
  "domainId": "550e8400-e29b-41d4-a716-446655440000",
  "domainName": "文档知识域",
  "domainType": "DOCUMENT",
  "description": "用于存储文档的知识域",
  "storagePath": "data/knowledge-network/domains/550e8400-e29b-41d4-a716-446655440000/storage",
  "ragIndexPath": "data/knowledge-network/domains/550e8400-e29b-41d4-a716-446655440000/rag-index",
  "config": {
    "ragBackend": "lucene",
    "chunkSize": 512
  },
  "status": "ACTIVE",
  "linkedEntityId": null,
  "createdAt": "2025-12-27T10:30:00",
  "updatedAt": "2025-12-27T10:30:00"
}
```

**cURL 示例：**
```bash
curl -X POST http://localhost:8080/api/knowledge-domains \
  -H "Content-Type: application/json" \
  -d '{
    "domainName": "文档知识域",
    "domainType": "DOCUMENT",
    "description": "用于存储文档的知识域"
  }'
```

---

### 2. 获取知识域详情

**GET** `/api/knowledge-domains/{domainId}`

**响应：**
```json
{
  "domainId": "550e8400-e29b-41d4-a716-446655440000",
  "domainName": "文档知识域",
  "domainType": "DOCUMENT",
  ...
}
```

**cURL 示例：**
```bash
curl http://localhost:8080/api/knowledge-domains/550e8400-e29b-41d4-a716-446655440000
```

---

### 3. 列出所有知识域

**GET** `/api/knowledge-domains`

**查询参数：**
- `type` (可选): 域类型 (`DOCUMENT`, `SOURCE_CODE`, `ROLE_KNOWLEDGE`, `API_DOCUMENTATION`, `MIXED`)
- `status` (可选): 域状态 (`ACTIVE`, `INACTIVE`, `ARCHIVED`, `ERROR`)

**响应：**
```json
[
  {
    "domainId": "550e8400-e29b-41d4-a716-446655440000",
    "domainName": "文档知识域",
    "domainType": "DOCUMENT",
    ...
  },
  {
    "domainId": "660e8400-e29b-41d4-a716-446655440001",
    "domainName": "源码知识域",
    "domainType": "SOURCE_CODE",
    ...
  }
]
```

**cURL 示例：**
```bash
# 列出所有域
curl http://localhost:8080/api/knowledge-domains

# 只列出文档类型的域
curl "http://localhost:8080/api/knowledge-domains?type=DOCUMENT"

# 只列出活跃状态的域
curl "http://localhost:8080/api/knowledge-domains?status=ACTIVE"
```

---

### 4. 更新知识域

**PUT** `/api/knowledge-domains/{domainId}`

**请求体：**
```json
{
  "domainName": "更新后的名称",
  "description": "更新后的描述",
  "status": "INACTIVE"
}
```

**响应：**
```json
{
  "domainId": "550e8400-e29b-41d4-a716-446655440000",
  "domainName": "更新后的名称",
  "description": "更新后的描述",
  "status": "INACTIVE",
  ...
}
```

**cURL 示例：**
```bash
curl -X PUT http://localhost:8080/api/knowledge-domains/550e8400-e29b-41d4-a716-446655440000 \
  -H "Content-Type: application/json" \
  -d '{
    "domainName": "更新后的名称",
    "description": "更新后的描述"
  }'
```

---

### 5. 删除知识域

**DELETE** `/api/knowledge-domains/{domainId}`

**响应：**
```json
{
  "success": true,
  "message": "Domain deleted successfully",
  "domainId": "550e8400-e29b-41d4-a716-446655440000"
}
```

**cURL 示例：**
```bash
curl -X DELETE http://localhost:8080/api/knowledge-domains/550e8400-e29b-41d4-a716-446655440000
```

---

### 6. 获取统计信息

**GET** `/api/knowledge-domains/statistics`

**响应：**
```json
{
  "totalDomains": 10,
  "documentDomains": 5,
  "sourceCodeDomains": 3,
  "roleKnowledgeDomains": 2
}
```

**cURL 示例：**
```bash
curl http://localhost:8080/api/knowledge-domains/statistics
```

---

## 🔧 配置

### application.yml

```yaml
omni-agent:
  knowledge-registry:
    type: file  # 使用文件存储（默认）
    file:
      base-path: data/knowledge-network/registry
      auto-create-directories: true
      pretty-print: true
```

---

## 💻 Java 客户端示例

### 使用 RestTemplate

```java
@Service
@RequiredArgsConstructor
public class KnowledgeDomainClient {
    
    private final RestTemplate restTemplate;
    private final String baseUrl = "http://localhost:8080/api/knowledge-domains";
    
    /**
     * 创建知识域
     */
    public KnowledgeDomain createDomain(String name, DomainType type) {
        CreateDomainRequest request = CreateDomainRequest.builder()
                .domainName(name)
                .domainType(type)
                .description("描述")
                .build();
        
        return restTemplate.postForObject(baseUrl, request, KnowledgeDomain.class);
    }
    
    /**
     * 获取知识域
     */
    public KnowledgeDomain getDomain(String domainId) {
        return restTemplate.getForObject(
                baseUrl + "/" + domainId, 
                KnowledgeDomain.class
        );
    }
    
    /**
     * 列出所有域
     */
    public List<KnowledgeDomain> listDomains() {
        KnowledgeDomain[] domains = restTemplate.getForObject(
                baseUrl, 
                KnowledgeDomain[].class
        );
        return Arrays.asList(domains);
    }
    
    /**
     * 更新域
     */
    public KnowledgeDomain updateDomain(String domainId, UpdateDomainRequest request) {
        restTemplate.put(baseUrl + "/" + domainId, request);
        return getDomain(domainId);
    }
    
    /**
     * 删除域
     */
    public void deleteDomain(String domainId) {
        restTemplate.delete(baseUrl + "/" + domainId);
    }
}
```

---

## 🌐 前端示例

### 使用 Fetch API

```javascript
// 创建知识域
async function createDomain() {
    const response = await fetch('/api/knowledge-domains', {
        method: 'POST',
        headers: {
            'Content-Type': 'application/json'
        },
        body: JSON.stringify({
            domainName: '文档知识域',
            domainType: 'DOCUMENT',
            description: '用于存储文档的知识域'
        })
    });
    
    const domain = await response.json();
    console.log('创建成功:', domain);
    return domain;
}

// 获取所有域
async function listDomains() {
    const response = await fetch('/api/knowledge-domains');
    const domains = await response.json();
    console.log('域列表:', domains);
    return domains;
}

// 获取统计信息
async function getStatistics() {
    const response = await fetch('/api/knowledge-domains/statistics');
    const stats = await response.json();
    console.log('统计信息:', stats);
    return stats;
}

// 删除域
async function deleteDomain(domainId) {
    const response = await fetch(`/api/knowledge-domains/${domainId}`, {
        method: 'DELETE'
    });
    
    const result = await response.json();
    console.log('删除结果:', result);
    return result;
}
```

### 使用 Axios

```javascript
import axios from 'axios';

const api = axios.create({
    baseURL: 'http://localhost:8080/api'
});

// 创建知识域
export const createDomain = async (domainData) => {
    const response = await api.post('/knowledge-domains', domainData);
    return response.data;
};

// 获取所有域
export const listDomains = async (type = null, status = null) => {
    const params = {};
    if (type) params.type = type;
    if (status) params.status = status;
    
    const response = await api.get('/knowledge-domains', { params });
    return response.data;
};

// 获取域详情
export const getDomain = async (domainId) => {
    const response = await api.get(`/knowledge-domains/${domainId}`);
    return response.data;
};

// 更新域
export const updateDomain = async (domainId, updateData) => {
    const response = await api.put(`/knowledge-domains/${domainId}`, updateData);
    return response.data;
};

// 删除域
export const deleteDomain = async (domainId) => {
    const response = await api.delete(`/knowledge-domains/${domainId}`);
    return response.data;
};

// 获取统计信息
export const getStatistics = async () => {
    const response = await api.get('/knowledge-domains/statistics');
    return response.data;
};
```

---

## 🧪 测试示例

### Postman 测试集合

```json
{
  "info": {
    "name": "Knowledge Registry API",
    "schema": "https://schema.getpostman.com/json/collection/v2.1.0/collection.json"
  },
  "item": [
    {
      "name": "创建知识域",
      "request": {
        "method": "POST",
        "header": [
          {
            "key": "Content-Type",
            "value": "application/json"
          }
        ],
        "body": {
          "mode": "raw",
          "raw": "{\n  \"domainName\": \"测试域\",\n  \"domainType\": \"DOCUMENT\",\n  \"description\": \"测试描述\"\n}"
        },
        "url": {
          "raw": "http://localhost:8080/api/knowledge-domains",
          "protocol": "http",
          "host": ["localhost"],
          "port": "8080",
          "path": ["api", "knowledge-domains"]
        }
      }
    }
  ]
}
```

---

## 📊 响应状态码

| 状态码 | 说明 |
|--------|------|
| 200 OK | 请求成功 |
| 201 Created | 创建成功 |
| 400 Bad Request | 请求参数错误 |
| 404 Not Found | 资源不存在 |
| 500 Internal Server Error | 服务器内部错误 |

---

**更新时间：** 2025-12-27  
**API 版本：** 1.0.0

