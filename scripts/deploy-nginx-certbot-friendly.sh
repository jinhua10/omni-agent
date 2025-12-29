#!/bin/bash

# OmniAgent Nginx配置部署脚本（Certbot友好版）
# 使用分离的配置结构，避免Certbot破坏主配置

set -e

echo "======================================"
echo "🚀 OmniAgent Nginx 配置部署（Certbot友好）"
echo "======================================"
echo ""

# 检查root权限
if [ "$EUID" -ne 0 ]; then
    echo "❌ 请使用root权限运行"
    echo "   使用: sudo bash $0"
    exit 1
fi

# 配置文件路径
MAIN_CONF_SOURCE="./nginx-production.conf"
SITE_CONF_SOURCE="./nginx-site-omni-agent.conf"
MAIN_CONF_TARGET="/etc/nginx/nginx.conf"
SITE_CONF_TARGET="/etc/nginx/sites-available/omni-agent"
SITE_CONF_LINK="/etc/nginx/sites-enabled/omni-agent"

echo "【1/8】备份现有配置..."
timestamp=$(date +%Y%m%d_%H%M%S)
if [ -f "$MAIN_CONF_TARGET" ]; then
    cp "$MAIN_CONF_TARGET" "/etc/nginx/nginx.conf.backup.$timestamp"
    echo "✅ 主配置已备份"
fi
if [ -f "$SITE_CONF_TARGET" ]; then
    cp "$SITE_CONF_TARGET" "$SITE_CONF_TARGET.backup.$timestamp"
    echo "✅ 站点配置已备份"
fi
echo ""

echo "【2/8】部署主配置文件..."
if [ -f "$MAIN_CONF_SOURCE" ]; then
    cp "$MAIN_CONF_SOURCE" "$MAIN_CONF_TARGET"
    echo "✅ 主配置文件已部署到: $MAIN_CONF_TARGET"
else
    echo "❌ 源文件不存在: $MAIN_CONF_SOURCE"
    exit 1
fi
echo ""

echo "【3/8】部署站点配置文件..."
if [ -f "$SITE_CONF_SOURCE" ]; then
    cp "$SITE_CONF_SOURCE" "$SITE_CONF_TARGET"
    echo "✅ 站点配置已部署到: $SITE_CONF_TARGET"
else
    echo "❌ 源文件不存在: $SITE_CONF_SOURCE"
    exit 1
fi
echo ""

echo "【4/8】创建软链接..."
if [ -L "$SITE_CONF_LINK" ]; then
    echo "ℹ️  软链接已存在，跳过"
else
    ln -s "$SITE_CONF_TARGET" "$SITE_CONF_LINK"
    echo "✅ 软链接已创建"
fi
echo ""

echo "【5/8】创建前端目录..."
mkdir -p /var/www/omni-agent/ui
echo "✅ 目录已创建"
echo ""

echo "【6/8】设置权限..."
chown -R www-data:www-data /var/www/omni-agent
chmod -R 755 /var/www/omni-agent
echo "✅ 权限已设置"
echo ""

echo "【7/8】测试Nginx配置..."
if nginx -t; then
    echo "✅ 配置测试通过"
else
    echo "❌ 配置测试失败"
    echo "   恢复备份: cp /etc/nginx/nginx.conf.backup.$timestamp /etc/nginx/nginx.conf"
    exit 1
fi
echo ""

echo "【8/8】重启Nginx..."
systemctl restart nginx
systemctl enable nginx
echo "✅ Nginx已重启"
echo ""

echo "======================================"
echo "✅ 部署完成"
echo "======================================"
echo ""
echo "📋 配置文件位置:"
echo "  - 主配置: $MAIN_CONF_TARGET（全局设置，Certbot不会修改）"
echo "  - 站点配置: $SITE_CONF_TARGET（Certbot会修改此文件）"
echo "  - 软链接: $SITE_CONF_LINK"
echo ""
echo "🔒 申请SSL证书:"
echo "  sudo certbot --nginx -d yumbo.top -d www.yumbo.top"
echo ""
echo "   Certbot会自动修改 $SITE_CONF_TARGET"
echo "   主配置文件 $MAIN_CONF_TARGET 不会被修改"
echo ""
echo "📝 证书续期（自动）:"
echo "  certbot会自动续期，配置文件不会损坏"
echo ""
echo "🔍 验证部署:"
echo "  curl -I http://localhost"
echo "  curl -I http://yumbo.top"
echo ""

