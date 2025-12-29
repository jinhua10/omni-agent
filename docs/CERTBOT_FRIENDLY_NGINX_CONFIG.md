# Certbot友好的Nginx配置方案

## 🎯 问题与解决方案

### ❌ 原来的问题

当您运行 `sudo certbot --nginx -d yumbo.top -d www.yumbo.top` 时：

1. **Certbot直接修改 `/etc/nginx/nginx.conf`**
2. 可能**覆盖或破坏**您的自定义配置
3. 如果证书申请失败，配置可能损坏
4. **每次续期都可能引入配置变更**

### ✅ 新的解决方案

**配置分离架构**：

```
/etc/nginx/
├── nginx.conf              # 主配置（全局设置，Certbot不碰）
└── sites-available/
    └── omni-agent          # 站点配置（Certbot只修改这个）
        └─ linked to sites-enabled/
```

**优势**：
- ✅ 主配置永不被修改
- ✅ 站点配置被Certbot管理
- ✅ 配置更清晰，易于维护
- ✅ 证书续期不会破坏配置

## 📁 文件说明

### 1. `nginx-production.conf` - 主配置文件

**位置**: `/etc/nginx/nginx.conf`

**内容**: 仅包含全局配置
- worker进程设置
- SSL全局配置
- 日志格式
- Gzip压缩
- 限流规则
- WebSocket映射

**关键**: Certbot**永远不会修改**此文件

### 2. `nginx-site-omni-agent.conf` - 站点配置

**位置**: `/etc/nginx/sites-available/omni-agent`

**内容**: OmniAgent具体配置
- 监听端口（80）
- 域名配置
- 前端路由
- API代理
- 健康检查

**关键**: Certbot**只修改**此文件，添加HTTPS配置

## 🚀 部署步骤

### 方式1: 使用自动化脚本（推荐）

```bash
# 1. 上传文件到服务器
scp nginx-production.conf root@yumbo.top:/tmp/
scp nginx-site-omni-agent.conf root@yumbo.top:/tmp/
scp scripts/deploy-nginx-certbot-friendly.sh root@yumbo.top:/tmp/

# 2. SSH登录服务器
ssh root@yumbo.top

# 3. 运行部署脚本
cd /tmp
sudo bash deploy-nginx-certbot-friendly.sh
```

### 方式2: 手动部署

```bash
# 1. 备份现有配置
sudo cp /etc/nginx/nginx.conf /etc/nginx/nginx.conf.backup.$(date +%Y%m%d)

# 2. 部署主配置
sudo cp nginx-production.conf /etc/nginx/nginx.conf

# 3. 创建sites目录（如果不存在）
sudo mkdir -p /etc/nginx/sites-available
sudo mkdir -p /etc/nginx/sites-enabled

# 4. 部署站点配置
sudo cp nginx-site-omni-agent.conf /etc/nginx/sites-available/omni-agent

# 5. 创建软链接
sudo ln -sf /etc/nginx/sites-available/omni-agent /etc/nginx/sites-enabled/omni-agent

# 6. 删除默认站点（可选）
sudo rm -f /etc/nginx/sites-enabled/default

# 7. 创建前端目录
sudo mkdir -p /var/www/omni-agent/ui

# 8. 设置权限
sudo chown -R www-data:www-data /var/www/omni-agent
sudo chmod -R 755 /var/www/omni-agent

# 9. 测试配置
sudo nginx -t

# 10. 重启Nginx
sudo systemctl restart nginx
```

## 🔒 申请SSL证书

### 首次申请

```bash
sudo certbot --nginx -d yumbo.top -d www.yumbo.top
```

**Certbot会做什么**：
1. 验证域名所有权
2. 申请Let's Encrypt证书
3. **自动修改** `/etc/nginx/sites-available/omni-agent`
4. 添加HTTPS配置（listen 443）
5. 添加HTTP到HTTPS重定向
6. 重新加载Nginx

**修改后的站点配置示例**：

```nginx
# HTTP服务器（重定向到HTTPS）
server {
    listen 80;
    server_name yumbo.top www.yumbo.top;
    
    # Certbot添加的重定向
    return 301 https://$server_name$request_uri;
}

# HTTPS服务器（Certbot自动添加）
server {
    listen 443 ssl;
    server_name yumbo.top www.yumbo.top;
    
    # Certbot添加的证书配置
    ssl_certificate /etc/letsencrypt/live/yumbo.top/fullchain.pem;
    ssl_certificate_key /etc/letsencrypt/live/yumbo.top/privkey.pem;
    include /etc/letsencrypt/options-ssl-nginx.conf;
    ssl_dhparam /etc/letsencrypt/ssl-dhparams.pem;
    
    # 您原来的location配置保持不变
    root /var/www/omni-agent/ui;
    # ...
}
```

### 证书续期

**自动续期**（推荐）：
```bash
# Certbot会自动设置cron任务
# 检查定时任务
sudo cat /etc/cron.d/certbot

# 手动测试续期（不会真正续期）
sudo certbot renew --dry-run
```

**手动续期**：
```bash
sudo certbot renew
```

**续期时Certbot做什么**：
- ✅ 更新证书文件（在 `/etc/letsencrypt/live/yumbo.top/`）
- ✅ **不修改**Nginx配置文件（证书路径不变）
- ✅ 重新加载Nginx

## 📋 证书文件位置（固定）

```bash
/etc/letsencrypt/
├── live/
│   └── yumbo.top/
│       ├── fullchain.pem     # 证书链（固定路径）
│       ├── privkey.pem       # 私钥（固定路径）
│       ├── cert.pem          # 证书
│       └── chain.pem         # 中间证书
├── archive/                  # 历史版本（自动管理）
├── renewal/                  # 续期配置
└── renewal-hooks/            # 续期钩子
```

**重要**：
- `live/yumbo.top/` 下的文件是**符号链接**
- 实际文件在 `archive/yumbo.top/`
- Certbot续期时会更新符号链接指向新证书
- **Nginx配置中的路径永远不变**

## ✅ 验证部署

### 1. 检查配置文件

```bash
# 主配置
sudo cat /etc/nginx/nginx.conf | head -20

# 站点配置
sudo cat /etc/nginx/sites-available/omni-agent | head -20

# 软链接
ls -l /etc/nginx/sites-enabled/omni-agent
```

### 2. 测试配置

```bash
sudo nginx -t
```

**预期输出**：
```
nginx: the configuration file /etc/nginx/nginx.conf syntax is ok
nginx: configuration file /etc/nginx/nginx.conf test is successful
```

### 3. 检查监听端口

```bash
sudo netstat -tlnp | grep nginx
```

**预期输出**：
```
tcp  0.0.0.0:80     LISTEN  1234/nginx: master
tcp  0.0.0.0:443    LISTEN  1234/nginx: master  # 如果已配置HTTPS
```

### 4. 测试HTTP访问

```bash
curl -I http://yumbo.top
```

**预期**（未配置HTTPS）：
```
HTTP/1.1 200 OK
```

**预期**（已配置HTTPS）：
```
HTTP/1.1 301 Moved Permanently
Location: https://yumbo.top/
```

### 5. 测试HTTPS访问（如果已配置）

```bash
curl -I https://yumbo.top
```

**预期**：
```
HTTP/2 200
```

## 🛠️ 常见问题

### Q1: Certbot修改了主配置怎么办？

**A**: 这不应该发生。如果发生了：

```bash
# 恢复主配置
sudo cp /etc/nginx/nginx.conf.backup.* /etc/nginx/nginx.conf

# 重新运行Certbot，指定站点配置
sudo certbot --nginx --cert-name yumbo.top
```

### Q2: 证书过期后怎么办？

**A**: Certbot会自动续期（90天证书，60天后自动续期）

检查续期配置：
```bash
sudo cat /etc/letsencrypt/renewal/yumbo.top.conf
```

### Q3: 如何查看证书到期时间？

```bash
sudo certbot certificates

# 或
sudo openssl x509 -in /etc/letsencrypt/live/yumbo.top/fullchain.pem -noout -dates
```

### Q4: 证书续期失败怎么办？

```bash
# 查看续期日志
sudo cat /var/log/letsencrypt/letsencrypt.log

# 手动续期
sudo certbot renew --force-renewal
```

### Q5: 如何强制使用HTTPS？

Certbot默认会添加HTTP到HTTPS的重定向。如果没有，手动添加：

```bash
# 编辑站点配置
sudo nano /etc/nginx/sites-available/omni-agent

# 在HTTP server块中添加
server {
    listen 80;
    server_name yumbo.top www.yumbo.top;
    return 301 https://$server_name$request_uri;
}
```

## 🔄 更新配置流程

### 更新站点配置

```bash
# 1. 备份当前配置
sudo cp /etc/nginx/sites-available/omni-agent /etc/nginx/sites-available/omni-agent.backup

# 2. 编辑配置
sudo nano /etc/nginx/sites-available/omni-agent

# 3. 测试
sudo nginx -t

# 4. 重新加载
sudo nginx -s reload
```

### 更新主配置

```bash
# 1. 备份
sudo cp /etc/nginx/nginx.conf /etc/nginx/nginx.conf.backup

# 2. 编辑
sudo nano /etc/nginx/nginx.conf

# 3. 测试
sudo nginx -t

# 4. 重启（主配置改动需要重启）
sudo systemctl restart nginx
```

## 📊 配置结构对比

### ❌ 旧方式（不推荐）

```
/etc/nginx/nginx.conf
  └── 包含所有配置（全局+站点）
      └── Certbot直接修改此文件 ⚠️
```

### ✅ 新方式（推荐）

```
/etc/nginx/
├── nginx.conf                  # 全局配置（不被修改）✅
└── sites-available/
    └── omni-agent              # 站点配置（Certbot管理）✅
```

## 🎯 总结

### 关键点

1. **配置分离**: 主配置和站点配置分开
2. **Certbot友好**: Certbot只修改站点配置
3. **证书路径固定**: `/etc/letsencrypt/live/yumbo.top/` 路径永远不变
4. **自动续期**: 无需担心证书过期，Certbot自动处理

### 下次重新申请证书

```bash
# 即使重新申请，也不会破坏配置
sudo certbot --nginx -d yumbo.top -d www.yumbo.top --force-renewal
```

**保证**：
- ✅ 主配置文件不会被修改
- ✅ 站点配置的自定义部分不会丢失
- ✅ 只有证书相关的几行会更新

---

**现在您可以放心使用Certbot，不用担心配置被破坏！** 🎉

