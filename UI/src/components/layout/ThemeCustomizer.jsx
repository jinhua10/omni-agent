/**
 * 主题定制器组件 / Theme Customizer Component
 *
 * 允许用户自定义主题颜色和样式
 * Allows users to customize theme colors and styles
 *
 * @author AI Reviewer Team
 * @since 2025-12-12
 */

import React, { useState } from 'react';
import { Drawer, Space, Typography, ColorPicker, Button, message, Divider } from 'antd';
import { useLanguage } from '../../contexts/LanguageContext';
import { useTheme } from '../../contexts/ThemeContext';

const { Title, Text } = Typography;

/**
 * 主题定制器组件 / Theme Customizer Component
 */
function ThemeCustomizer({ open, onClose }) {
  const { t } = useLanguage();
  const { theme, themeName, setTheme, updateCustomTheme } = useTheme();

  const [customColors, setCustomColors] = useState({
    primary: theme.primary,
    background: theme.background,
    surface: theme.surface,
    text: theme.text,
    textSecondary: theme.textSecondary,
    border: theme.border,
  });

  // 处理颜色变化 / Handle color change
  const handleColorChange = (key, color) => {
    const hexColor = typeof color === 'string' ? color : color.toHexString();
    setCustomColors(prev => ({
      ...prev,
      [key]: hexColor,
    }));
  };

  // 应用自定义主题 / Apply custom theme
  const handleApply = () => {
    updateCustomTheme(customColors);
    setTheme('custom');
    message.success(t('theme.customizer.applySuccess') || '主题已应用');
  };

  // 重置为默认 / Reset to default
  const handleReset = () => {
    const defaultColors = {
      primary: '#1890ff',
      background: '#ffffff',
      surface: '#f5f5f5',
      text: '#333333',
      textSecondary: '#666666',
      border: '#d9d9d9',
    };
    setCustomColors(defaultColors);
    updateCustomTheme(defaultColors);
    message.info(t('theme.customizer.resetSuccess') || '已重置为默认主题');
  };

  return (
    <Drawer
      title={t('theme.customizer.title') || '主题定制器'}
      placement="right"
      onClose={onClose}
      open={open}
      size="default"
      styles={{ body: { width: 360 } }}
    >
      <Space direction="vertical" size="large" style={{ width: '100%' }}>
        {/* 主色调 / Primary color */}
        <div>
          <Text strong>{t('theme.customizer.primary') || '主色调'}</Text>
          <div style={{ marginTop: 8 }}>
            <ColorPicker
              value={customColors.primary}
              onChange={(color) => handleColorChange('primary', color)}
              showText
              style={{ width: '100%' }}
            />
          </div>
        </div>

        <Divider />

        {/* 背景色 / Background color */}
        <div>
          <Text strong>{t('theme.customizer.background') || '背景色'}</Text>
          <div style={{ marginTop: 8 }}>
            <ColorPicker
              value={customColors.background}
              onChange={(color) => handleColorChange('background', color)}
              showText
              style={{ width: '100%' }}
            />
          </div>
        </div>

        {/* 表面色 / Surface color */}
        <div>
          <Text strong>{t('theme.customizer.surface') || '表面色'}</Text>
          <div style={{ marginTop: 8 }}>
            <ColorPicker
              value={customColors.surface}
              onChange={(color) => handleColorChange('surface', color)}
              showText
              style={{ width: '100%' }}
            />
          </div>
        </div>

        <Divider />

        {/* 主要文本色 / Primary text color */}
        <div>
          <Text strong>{t('theme.customizer.text') || '主要文本色'}</Text>
          <div style={{ marginTop: 8 }}>
            <ColorPicker
              value={customColors.text}
              onChange={(color) => handleColorChange('text', color)}
              showText
              style={{ width: '100%' }}
            />
          </div>
        </div>

        {/* 次要文本色 / Secondary text color */}
        <div>
          <Text strong>{t('theme.customizer.textSecondary') || '次要文本色'}</Text>
          <div style={{ marginTop: 8 }}>
            <ColorPicker
              value={customColors.textSecondary}
              onChange={(color) => handleColorChange('textSecondary', color)}
              showText
              style={{ width: '100%' }}
            />
          </div>
        </div>

        {/* 边框色 / Border color */}
        <div>
          <Text strong>{t('theme.customizer.border') || '边框色'}</Text>
          <div style={{ marginTop: 8 }}>
            <ColorPicker
              value={customColors.border}
              onChange={(color) => handleColorChange('border', color)}
              showText
              style={{ width: '100%' }}
            />
          </div>
        </div>

        <Divider />

        {/* 操作按钮 / Action buttons */}
        <Space style={{ width: '100%' }}>
          <Button type="primary" onClick={handleApply} style={{ flex: 1 }}>
            {t('theme.customizer.apply') || '应用'}
          </Button>
          <Button onClick={handleReset} style={{ flex: 1 }}>
            {t('theme.customizer.reset') || '重置'}
          </Button>
        </Space>

        {/* 预览区域 / Preview area */}
        <div
          style={{
            padding: 16,
            borderRadius: 8,
            background: customColors.background,
            border: `1px solid ${customColors.border}`,
          }}
        >
          <div
            style={{
              padding: 12,
              background: customColors.surface,
              borderRadius: 4,
              marginBottom: 8,
            }}
          >
            <Text style={{ color: customColors.text, display: 'block' }}>
              {t('theme.customizer.previewText') || '这是主要文本'}
            </Text>
            <Text style={{ color: customColors.textSecondary, display: 'block' }}>
              {t('theme.customizer.previewSecondary') || '这是次要文本'}
            </Text>
          </div>
          <Button type="primary" style={{ background: customColors.primary }}>
            {t('theme.customizer.previewButton') || '主要按钮'}
          </Button>
        </div>
      </Space>
    </Drawer>
  );
}

export default ThemeCustomizer;

