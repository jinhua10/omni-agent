# API 模块

API 接口封装目录，按功能模块拆分。

## 目录结构

```
api/
├── index.js           # Axios 实例和拦截器配置
└── modules/           # 模块化 API
    ├── document.js    # 文档 API
    ├── qa.js          # 问答 API
    ├── role.js        # 角色 API
    ├── feedback.js    # 反馈 API
    ├── hope.js        # HOPE API
    ├── wish.js        # 愿望单 API
    └── admin.js       # 管理 API
```

## 使用示例

```javascript
import api from '@api'

// 调用问答 API
const response = await api.qa.ask('你好')
```

