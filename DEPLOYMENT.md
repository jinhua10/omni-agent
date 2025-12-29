# 前后端分离部署指南

## 部署架构

```
用户浏览器
    ↓
Nginx (80/443端口)
    ↓
    ├─→ 前端静态资源 (/usr/share/nginx/html)
    └─→ 后端API代理 (localhost:8080)
```

## 跨域问题处理

### 当前状态
✅ **后端已配置完善的CORS支持**

项目在 `CorsConfig.java` 中已经配置了：
- 允许所有来源 (`*`)
- 允许所有请求方法 (GET, POST, PUT, DELETE等)
- 允许所有请求头
- 支持携带Cookie
- 暴露所有响应头

### 两种部署方案

#### 方案一：Nginx反向代理（推荐）
使用Nginx代理API请求到后端，前端和API在同一域名下，**无跨域问题**。

**优点：**
- 无跨域问题
- 统一域名管理
- 可以添加SSL/TLS加密
- 可以做负载均衡
- 可以配置访问控制

**配置步骤：**

1. **前端构建配置** (`UI/vite.config.js`)
```javascript
export default defineConfig({
  base: '/',  // 部署到根路径
  server: {
    proxy: {
      '/api': {
        target: 'http://localhost:8080',
        changeOrigin: true
      }
    }
  },
  build: {
    outDir: 'dist'
  }
})
```

2. **构建前端**
```bash
cd UI
npm install
npm run build
```

3. **部署到Nginx**
```bash
# 复制前端文件到nginx目录
cp -r dist/* /usr/share/nginx/html/

# 复制nginx配置
cp nginx.conf.example /etc/nginx/conf.d/omni-agent.conf

# 修改配置中的域名和路径
vim /etc/nginx/conf.d/omni-agent.conf

# 测试配置
nginx -t

# 重启nginx
systemctl restart nginx
```

4. **启动后端服务**
```bash
cd omni-agent-web
mvn clean package -DskipTests
java -jar target/omni-agent-web-1.0.0.jar
```

#### 方案二：直接跨域访问
前端直接访问后端API，依赖后端的CORS配置。

**优点：**
- 部署简单
- 前后端完全独立

**缺点：**
- 存在跨域限制
- 不便于统一管理
- 无法添加nginx层的安全控制

**配置步骤：**

1. **前端环境变量配置** (`.env.production`)
```env
VITE_API_BASE_URL=http://your-backend-domain:8080
```

2. **后端application.yml确认配置**
```yaml
server:
  port: 8080
  
# CORS已在CorsConfig.java中配置，无需额外配置
```

## Nginx配置说明

### 基础配置项

```nginx
server {
    listen 80;
    server_name your-domain.com;
    
    # 前端静态资源
    location / {
        root /usr/share/nginx/html;
        index index.html;
        try_files $uri $uri/ /index.html;
    }
    
    # API反向代理
    location /api/ {
        proxy_pass http://localhost:8080;
        proxy_set_header Host $host;
        proxy_set_header X-Real-IP $remote_addr;
        proxy_set_header X-Forwarded-For $proxy_add_x_forwarded_for;
    }
    
    # WebSocket代理
    location /ws/ {
        proxy_pass http://localhost:8080;
        proxy_http_version 1.1;
        proxy_set_header Upgrade $http_upgrade;
        proxy_set_header Connection "upgrade";
    }
}
```

### 关键配置项解释

1. **try_files $uri $uri/ /index.html**
   - 支持Vue Router的history模式
   - 所有不存在的路径都返回index.html

2. **proxy_pass http://localhost:8080**
   - 将/api/开头的请求转发到后端
   - 后端无需修改，仍监听8080端口

3. **WebSocket配置**
   - `proxy_http_version 1.1` 启用HTTP/1.1
   - `Upgrade` 和 `Connection` 头支持WebSocket升级

4. **文件上传大小**
   - `client_max_body_size 100M` 允许上传最大100MB文件

## 生产环境建议

### 1. 使用HTTPS
```bash
# 使用Let's Encrypt免费证书
sudo apt install certbot python3-certbot-nginx
sudo certbot --nginx -d your-domain.com
```

### 2. 启用HTTP/2
```nginx
listen 443 ssl http2;
```

### 3. 配置安全头
```nginx
add_header X-Frame-Options "SAMEORIGIN" always;
add_header X-Content-Type-Options "nosniff" always;
add_header X-XSS-Protection "1; mode=block" always;
add_header Strict-Transport-Security "max-age=31536000" always;
```

### 4. 后端健康检查
```nginx
location /health {
    proxy_pass http://localhost:8080/actuator/health;
    access_log off;
}
```

### 5. 日志配置
```nginx
access_log /var/log/nginx/omni-agent-access.log;
error_log /var/log/nginx/omni-agent-error.log;
```

## 验证部署

### 1. 检查前端
```bash
curl http://your-domain.com
# 应返回index.html内容
```

### 2. 检查API代理
```bash
curl http://your-domain.com/api/system/info
# 应返回后端API响应
```

### 3. 检查WebSocket
```javascript
const ws = new WebSocket('ws://your-domain.com/ws/progress');
ws.onopen = () => console.log('WebSocket连接成功');
```

## 常见问题

### 1. 404错误
**现象：** 刷新页面时返回404

**解决：** 确保nginx配置了 `try_files $uri $uri/ /index.html;`

### 2. API请求失败
**现象：** 控制台显示CORS错误或连接失败

**检查：**
- 后端服务是否启动 (`curl http://localhost:8080/api/system/info`)
- Nginx配置是否正确 (`nginx -t`)
- 防火墙是否开放端口

### 3. WebSocket连接失败
**检查：**
- Nginx是否配置了WebSocket代理
- 是否设置了正确的Upgrade头
- 超时时间是否足够长

### 4. 文件上传失败
**解决：** 增加nginx的 `client_max_body_size`
```nginx
client_max_body_size 100M;
```

## 监控和维护

### 查看Nginx日志
```bash
tail -f /var/log/nginx/omni-agent-access.log
tail -f /var/log/nginx/omni-agent-error.log
```

### 查看后端日志
```bash
tail -f logs/omni-agent.log
```

### 重启服务
```bash
# 重启nginx
sudo systemctl restart nginx

# 重启后端（使用systemd）
sudo systemctl restart omni-agent
```

## Docker部署（可选）

如果使用Docker部署，可以创建docker-compose.yml：

```yaml
version: '3.8'
services:
  nginx:
    image: nginx:alpine
    ports:
      - "80:80"
      - "443:443"
    volumes:
      - ./nginx.conf:/etc/nginx/conf.d/default.conf
      - ./UI/dist:/usr/share/nginx/html
    depends_on:
      - backend
      
  backend:
    build: .
    ports:
      - "8080:8080"
    environment:
      - SPRING_PROFILES_ACTIVE=prod
```

## 总结

✅ **推荐使用方案一（Nginx反向代理）**
- 后端CORS配置已就绪
- Nginx配置简单有效
- 统一域名管理
- 便于后续扩展和维护

📝 **部署清单：**
- [x] 后端CORS配置完成
- [ ] 前端构建配置
- [ ] Nginx配置文件
- [ ] 域名和SSL证书
- [ ] 后端服务启动
- [ ] 部署验证测试
