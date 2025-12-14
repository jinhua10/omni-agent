# AI Reviewer Frontend

基于 React + Vite 的现代化前端项目。

## 📂 目录结构

```
UI/
├── src/
│   ├── api/                    # API 接口封装
│   │   ├── modules/            # 按模块拆分的 API
│   │   └── index.js            # Axios 实例配置
│   ├── assets/                 # 静态资源
│   │   ├── css/                # 样式文件
│   │   └── images/             # 图片资源
│   ├── components/             # React 组件
│   │   ├── common/             # 通用组件
│   │   ├── layout/             # 布局组件
│   │   ├── qa/                 # 问答模块
│   │   ├── document/           # 文档模块
│   │   ├── statistics/         # 统计模块
│   │   ├── feedback/           # 反馈系统
│   │   ├── role/               # 角色管理（Phase 8）
│   │   ├── wish/               # 愿望单（Phase 8）
│   │   ├── collaboration/      # 协作网络（Phase 9）
│   │   ├── ai-service/         # AI 服务市场（Phase 9）
│   │   ├── user/               # 个人中心（Phase 9）
│   │   └── admin/              # 系统管理（Phase 9）
│   ├── contexts/               # React Context
│   ├── hooks/                  # 自定义 Hooks
│   ├── lang/                   # 国际化翻译
│   ├── utils/                  # 工具函数
│   ├── App.jsx                 # 主应用组件
│   ├── main.jsx                # 应用入口
│   └── index.html              # HTML 模板
├── package.json                # 项目配置
├── vite.config.js              # Vite 配置
├── .eslintrc.json              # ESLint 配置
├── .gitignore                  # Git 忽略文件
└── README.md                   # 本文件
```

## 🚀 快速开始

### 安装依赖
```bash
npm install
```

### 开发模式
```bash
npm run dev
```
访问：http://localhost:3000

### 生产构建
```bash
npm run build
```
构建产物输出到：`../src/main/resources/static/`

### 代码检查
```bash
npm run lint
```

## 🔧 配置说明

### API 代理
开发模式下，所有 `/api` 请求会自动代理到 `http://localhost:8080`（Spring Boot 后端）。

### 路径别名
- `@` → `src/`
- `@components` → `src/components/`
- `@api` → `src/api/`
- `@hooks` → `src/hooks/`
- `@contexts` → `src/contexts/`
- `@utils` → `src/utils/`
- `@styles` → `src/styles/`
- `@assets` → `src/assets/`

### 使用示例
```javascript
import Button from '@components/common/Button'
import { useApi } from '@hooks/useApi'
import api from '@api'
```

## 📋 开发规范

请遵守 `docs/refactor/20251209-23-00-00-CODE_STANDARDS.md` 中的编码规范。

### 关键规范
1. ✅ 组件必须使用 JSX 格式
2. ✅ 组件文件使用 `.jsx` 扩展名
3. ✅ 样式必须提取到独立 CSS 文件
4. ✅ 禁止内联样式（特殊情况除外）
5. ✅ 使用 BEM 命名法命名 CSS 类
6. ✅ 注释使用中英文双语格式

## 🎯 当前进度

- [x] Phase 7.1: 前端项目初始化 ✅
- [ ] Phase 7.2: 目录结构重构
- [ ] Phase 7.3: 通用组件扩充
- [ ] Phase 7.4: API 接口重构
- [ ] Phase 7.5: 状态管理设计
- [ ] Phase 7.6: 样式系统完善

## 📚 技术栈

- **框架**: React 18
- **构建工具**: Vite 5
- **HTTP 库**: Axios
- **Markdown**: Marked.js
- **代码高亮**: Highlight.js
- **状态管理**: React Context + Hooks
- **样式方案**: CSS Modules / 独立 CSS

---

**文档版本**: v1.0  
**创建日期**: 2025-12-12  
**作者**: AI Reviewer Team

