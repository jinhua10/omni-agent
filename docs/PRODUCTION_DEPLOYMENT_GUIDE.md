# 🚀 生产环境部署指南

## 问题修复

### ✅ 已修复的问题

**问题描述**: 部署到生产环境后，首页底部出现错误：
```
TypeError: Cannot add property current, object is not extensible
```

**根本原因**: 
1. 使用 `eval(stat.icon)` 动态获取图标组件，在生产环境压缩代码后会导致对象不可扩展错误
2. 生产环境启用了sourcemap但未正确配置

**修复方案**:
1. ✅ 使用 `iconMap` 对象映射代替 `eval()`
2. ✅ 优化生产构建配置，禁用sourcemap
3. ✅ 添加代码分包优化

---

## 📦 前端构建

### 1. 本地构建

```bash
cd D:\Jetbrains\omni-agent\UI

# 安装依赖（首次或依赖更新时）
npm install

# 生产构建
npm run build
```

构建完成后，文件生成在 `UI/dist` 目录。

### 2. 构建产物

```
dist/
├── index.html              # 入口HTML
├── assets/
│   ├── css/               # CSS文件
│   ├── images/            # 图片资源
│   └── fonts/             # 字体文件
└── js/
    ├── index-[hash].js    # 主入口
    └── chunks/            # 代码分包
        ├── react-vendor-[hash].js   # React核心库
        ├── antd-vendor-[hash].js    # Ant Design组件库
        └── utils-[hash].js          # 工具库
```

---

## 🖥️ 服务器部署

### 1. 上传前端文件

```bash
# 使用 SFTP 或 SCP 上传
scp -r dist/* root@yumbo.top:/root/UI/dist/

# 或使用 rsync（推荐）
rsync -avz --delete dist/ root@yumbo.top:/root/UI/dist/
```

### 2. 服务器目录结构

```
/root/
├── UI/
│   └── dist/              # 前端构建产物
│       ├── index.html
│       ├── assets/
│       └── js/
└── omni-agent/            # 后端JAR包（可选）
    └── omni-agent-web.jar
```

---

## 🔧 Nginx 配置

### 1. 配置文件位置

```bash
sudo nano /etc/nginx/nginx.conf
```

### 2. 完整配置

使用项目根目录的 `nginx-production.conf` 文件内容。

### 3. 关键配置说明

#### 前端静态文件
```nginx
root /root/UI/dist;
index index.html;

location / {
    try_files $uri $uri/ /index.html;
    add_header Cache-Control "no-cache";
}
```

#### 静态资源缓存
```nginx
location ~* \.(js|css|png|jpg|jpeg|gif|ico|svg|woff|woff2|ttf|eot)$ {
    expires 30d;
    add_header Cache-Control "public, immutable";
}
```

#### API代理
```nginx
location /api/ {
    proxy_pass http://127.0.0.1:8080;
    proxy_set_header Host $host;
    proxy_set_header X-Real-IP $remote_addr;
    proxy_set_header X-Forwarded-For $proxy_add_x_forwarded_for;
}
```

### 4. 重启Nginx

```bash
# 测试配置
sudo nginx -t

# 重新加载配置
sudo nginx -s reload

# 或重启服务
sudo systemctl restart nginx
```

---

## 🎯 后端部署

### 1. 打包后端

```bash
cd D:\Jetbrains\omni-agent

# Maven打包
mvn clean package -DskipTests

# JAR包位置
# omni-agent-example-basic/target/omni-agent-example-basic-1.0.0.jar
```

### 2. 上传并运行

```bash
# 上传JAR包
scp omni-agent-example-basic/target/*.jar root@yumbo.top:/root/omni-agent/

# SSH登录服务器
ssh root@yumbo.top

# 运行后端（使用8080端口）
cd /root/omni-agent
nohup java -jar omni-agent-example-basic-1.0.0.jar --server.port=8080 > app.log 2>&1 &

# 查看日志
tail -f app.log
```

### 3. 配置后端服务（可选 - systemd）

创建 `/etc/systemd/system/omni-agent.service`:

```ini
[Unit]
Description=OmniAgent Backend Service
After=network.target

[Service]
Type=simple
User=root
WorkingDirectory=/root/omni-agent
ExecStart=/usr/bin/java -jar omni-agent-example-basic-1.0.0.jar --server.port=8080
Restart=on-failure
RestartSec=10
StandardOutput=append:/root/omni-agent/app.log
StandardError=append:/root/omni-agent/app.log

[Install]
WantedBy=multi-user.target
```

启动服务：
```bash
sudo systemctl daemon-reload
sudo systemctl enable omni-agent
sudo systemctl start omni-agent
sudo systemctl status omni-agent
```

---

## ✅ 验证部署

### 1. 检查Nginx状态

```bash
sudo systemctl status nginx
sudo nginx -t
```

### 2. 检查后端服务

```bash
# 检查端口监听
sudo netstat -nltp | grep 8080

# 测试API
curl http://localhost:8080/api/health
```

### 3. 访问网站

```
http://yumbo.top
```

### 4. 检查错误日志

```bash
# Nginx错误日志
sudo tail -f /var/log/nginx/error.log

# 后端应用日志
tail -f /root/omni-agent/app.log
```

---

## 🐛 常见问题

### 1. 403 Forbidden

**原因**: Nginx没有读取权限

**解决**:
```bash
sudo chmod -R 755 /root/UI/dist
sudo chown -R www-data:www-data /root/UI/dist
```

### 2. 404 Not Found（刷新页面）

**原因**: 单页应用路由问题

**解决**: 确保nginx配置中有:
```nginx
location / {
    try_files $uri $uri/ /index.html;
}
```

### 3. API请求失败

**原因**: 后端服务未启动或端口不对

**解决**:
```bash
# 检查后端进程
ps aux | grep java

# 检查端口
sudo netstat -nltp | grep 8080
```

### 4. 主题引擎错误

**原因**: 代码压缩后的兼容性问题

**解决**: 已修复（移除eval，优化构建配置）

---

## 📝 部署检查清单

- [ ] 前端构建完成 (`npm run build`)
- [ ] 前端文件上传到服务器 (`/root/UI/dist`)
- [ ] Nginx配置正确
- [ ] Nginx重新加载配置
- [ ] 后端JAR包上传
- [ ] 后端服务运行（8080端口）
- [ ] 访问网站正常
- [ ] API请求正常
- [ ] 无控制台错误
- [ ] 日志无异常

---

## 🔄 更新流程

### 前端更新

```bash
# 本地
cd UI
npm run build

# 上传
rsync -avz --delete dist/ root@yumbo.top:/root/UI/dist/

# 服务器
# 无需重启nginx，浏览器强制刷新（Ctrl+F5）即可
```

### 后端更新

```bash
# 本地打包
mvn clean package -DskipTests

# 上传
scp omni-agent-example-basic/target/*.jar root@yumbo.top:/root/omni-agent/

# 服务器重启
ssh root@yumbo.top
sudo systemctl restart omni-agent
```

---

**部署日期**: 2025-12-29  
**最后更新**: 修复生产环境主题引擎错误

