/**
 * UI主题切换器组件 / UI Theme Switcher Component
 *
 * 允许用户浏览和切换不同的UI主题
 * Allows users to browse and switch between different UI themes
 *
 * @author AI Reviewer Team
 * @since 2025-12-12
 */

import React, { useState } from 'react';
import { Modal, Card, Row, Col, Button, Tag, Badge, Upload, message, Tabs } from 'antd';
import {
  AppstoreOutlined,
  DownloadOutlined,
  UploadOutlined,
  CheckCircleOutlined,
  ClockCircleOutlined,
  DeleteOutlined,
  ExportOutlined,
} from '@ant-design/icons';
import { useLanguage } from '../../contexts/LanguageContext';
import { useUIThemeEngine } from '../../contexts/UIThemeEngineContext';
import './ui-theme-switcher.css';

/**
 * UI主题切换器 / UI Theme Switcher
 */
function UIThemeSwitcher({ open, onClose }) {
  const { t, language } = useLanguage();
  const {
    currentUITheme,
    allThemes,
    builtinThemes,
    customThemes,
    switchUITheme,
    installCustomTheme,
    uninstallCustomTheme,
    exportThemeConfig,
    importThemeConfig,
    themeLoading,
  } = useUIThemeEngine();

  const [switchingTheme, setSwitchingTheme] = useState(null);

  // 处理主题切换 / Handle theme switch
  const handleThemeSwitch = async (themeId) => {
    setSwitchingTheme(themeId);
    const success = await switchUITheme(themeId);
    if (success) {
      message.success(t('uiTheme.switcher.switchSuccess') || '主题切换成功');
    } else {
      message.warning(t('uiTheme.switcher.themeInDev') || '该主题正在开发中，敬请期待');
    }
    setSwitchingTheme(null);
  };

  const [uploadToServer, setUploadToServer] = useState(true); // 默认上传到服务器 / Default upload to server
  const [uploadingTheme, setUploadingTheme] = useState(false);

  // 处理主题导入 / Handle theme import
  const handleImport = async (file) => {
    const reader = new FileReader();
    reader.onload = async (e) => {
      try {
        const themeData = JSON.parse(e.target.result);

        setUploadingTheme(true);

        // 如果选择上传到服务器 / If upload to server is selected
        const result = await installCustomTheme(themeData, {
          uploadToServer: uploadToServer,
          themeFiles: [], // 可以支持额外的主题文件 / Can support additional theme files
        });

        if (result.success) {
          message.success(
            uploadToServer
              ? t('uiTheme.switcher.uploadSuccess') || '主题已上传到服务器'
              : t('uiTheme.switcher.importSuccess') || '主题导入成功'
          );
        } else {
          message.error(result.error || t('uiTheme.switcher.importFailed') || '主题导入失败');
        }
      } catch (error) {
        console.error('Failed to import theme:', error);
        message.error(t('uiTheme.switcher.importFailed') || '主题导入失败');
      } finally {
        setUploadingTheme(false);
      }
    };
    reader.readAsText(file);
    return false; // 阻止自动上传 / Prevent auto upload
  };

  // 处理主题导出 / Handle theme export
  const handleExport = (themeId) => {
    const configJson = exportThemeConfig(themeId);
    if (configJson) {
      const blob = new Blob([configJson], { type: 'application/json' });
      const url = URL.createObjectURL(blob);
      const a = document.createElement('a');
      a.href = url;
      a.download = `theme-${themeId}.json`;
      a.click();
      URL.revokeObjectURL(url);
      message.success(t('uiTheme.switcher.exportSuccess') || '主题导出成功');
    }
  };

  // 处理主题卸载 / Handle theme uninstall
  const handleUninstall = (themeId) => {
    Modal.confirm({
      title: t('uiTheme.switcher.confirmUninstall') || '确认卸载',
      content: t('uiTheme.switcher.uninstallWarning') || '确定要卸载这个主题吗？',
      onOk: () => {
        const success = uninstallCustomTheme(themeId);
        if (success) {
          message.success(t('uiTheme.switcher.uninstallSuccess') || '主题卸载成功');
        }
      },
    });
  };

  // 渲染主题卡片 / Render theme card
  const renderThemeCard = (theme) => {
    const isActive = theme.id === currentUITheme;
    const isSwitching = switchingTheme === theme.id;
    const isAvailable = theme.status !== 'developing';
    const themeName = theme.name[language] || theme.name.zh;
    const themeDesc = theme.description[language] || theme.description.zh;

    return (
      <Card
        key={theme.id}
        hoverable={isAvailable}
        className={`ui-theme-card ${isActive ? 'ui-theme-card--active' : ''} ${!isAvailable ? 'ui-theme-card--disabled' : ''}`}
        cover={
          <div className="ui-theme-card__preview">
            {theme.preview ? (
              <img src={theme.preview} alt={themeName} />
            ) : (
              <div className="ui-theme-card__preview-placeholder">
                <AppstoreOutlined style={{ fontSize: 48 }} />
              </div>
            )}
            {isActive && (
              <div className="ui-theme-card__active-badge">
                <CheckCircleOutlined /> {t('uiTheme.switcher.current') || '当前'}
              </div>
            )}
            {!isAvailable && (
              <div className="ui-theme-card__dev-badge">
                <ClockCircleOutlined /> {t('uiTheme.switcher.developing') || '开发中'}
              </div>
            )}
          </div>
        }
        actions={[
          isAvailable ? (
            <Button
              type={isActive ? 'default' : 'primary'}
              onClick={() => handleThemeSwitch(theme.id)}
              loading={isSwitching}
              disabled={isActive}
            >
              {isActive ? t('uiTheme.switcher.current') || '当前' : t('uiTheme.switcher.apply') || '应用'}
            </Button>
          ) : (
            <Button disabled>
              {t('uiTheme.switcher.comingSoon') || '敬请期待'}
            </Button>
          ),
          theme.type === 'custom' && (
            <Button
              icon={<DeleteOutlined />}
              onClick={() => handleUninstall(theme.id)}
            >
              {t('uiTheme.switcher.uninstall') || '卸载'}
            </Button>
          ),
          <Button
            icon={<ExportOutlined />}
            onClick={() => handleExport(theme.id)}
          >
            {t('uiTheme.switcher.export') || '导出'}
          </Button>,
        ].filter(Boolean)}
      >
        <Card.Meta
          title={
            <div className="ui-theme-card__title">
              {themeName}
              <Tag color={theme.type === 'builtin' ? 'blue' : 'green'}>
                {theme.type === 'builtin' ? t('uiTheme.switcher.builtin') || '内置' : t('uiTheme.switcher.custom') || '自定义'}
              </Tag>
            </div>
          }
          description={
            <div className="ui-theme-card__description">
              <p>{themeDesc}</p>
              <div className="ui-theme-card__meta">
                <span>{t('uiTheme.switcher.version') || '版本'}: {theme.version}</span>
                <span>{t('uiTheme.switcher.author') || '作者'}: {theme.author}</span>
              </div>
            </div>
          }
        />
      </Card>
    );
  };

  // 构建 Tabs items / Build Tabs items
  const tabItems = [
    {
      key: 'builtin',
      label: (
        <span>
          <AppstoreOutlined />
          {t('uiTheme.switcher.builtinThemes') || '内置主题'} ({builtinThemes.length})
        </span>
      ),
      children: (
        <Row gutter={[16, 16]}>
          {builtinThemes.map(theme => (
            <Col xs={24} sm={12} md={8} key={theme.id}>
              {renderThemeCard(theme)}
            </Col>
          ))}
        </Row>
      ),
    },
    {
      key: 'custom',
      label: (
        <span>
          <DownloadOutlined />
          {t('uiTheme.switcher.customThemes') || '自定义主题'} ({customThemes.length})
        </span>
      ),
      children: customThemes.length > 0 ? (
        <Row gutter={[16, 16]}>
          {customThemes.map(theme => (
            <Col xs={24} sm={12} md={8} key={theme.id}>
              {renderThemeCard(theme)}
            </Col>
          ))}
        </Row>
      ) : (
        <div className="ui-theme-switcher__empty">
          <AppstoreOutlined style={{ fontSize: 64, color: '#ccc' }} />
          <p>{t('uiTheme.switcher.noCustomThemes') || '暂无自定义主题'}</p>
          <p>{t('uiTheme.switcher.importTip') || '点击下方按钮导入主题'}</p>
        </div>
      ),
    },
    {
      key: 'management',
      label: (
        <span>
          <UploadOutlined />
          {t('uiTheme.switcher.management') || '主题管理'}
        </span>
      ),
      children: (
        <div className="ui-theme-switcher__management">
          <Card title={t('uiTheme.switcher.importExport') || '导入/导出主题'}>
            <div className="ui-theme-switcher__actions">
              <div className="ui-theme-switcher__upload-section">
                <div className="ui-theme-switcher__upload-option">
                  <input
                    type="checkbox"
                    id="uploadToServer"
                    checked={uploadToServer}
                    onChange={(e) => setUploadToServer(e.target.checked)}
                  />
                  <label htmlFor="uploadToServer">
                    {t('uiTheme.switcher.uploadToServer') || '上传到服务器（推荐）'}
                  </label>
                  <span className="ui-theme-switcher__upload-tip">
                    {t('uiTheme.switcher.serverPersistTip') || '主题将被持久化到服务器静态资源目录'}
                  </span>
                </div>

                <Upload
                  accept=".json"
                  beforeUpload={handleImport}
                  showUploadList={false}
                  disabled={uploadingTheme}
                >
                  <Button
                    icon={<UploadOutlined />}
                    size="large"
                    loading={uploadingTheme}
                  >
                    {uploadingTheme
                      ? t('uiTheme.switcher.uploading') || '上传中...'
                      : t('uiTheme.switcher.importTheme') || '导入主题'}
                  </Button>
                </Upload>
              </div>

              <div className="ui-theme-switcher__info">
                <h4>{t('uiTheme.switcher.howToUse') || '如何使用'}</h4>
                <ol>
                  <li>{t('uiTheme.switcher.step1') || '从主题市场或AI生成获取主题文件'}</li>
                  <li>{t('uiTheme.switcher.step2') || '勾选"上传到服务器"（推荐）'}</li>
                  <li>{t('uiTheme.switcher.step3') || '点击"导入主题"按钮选择JSON文件'}</li>
                  <li>{t('uiTheme.switcher.step4') || '导入成功后在"自定义主题"标签页应用'}</li>
                  <li>{t('uiTheme.switcher.step5') || '上传到服务器的主题会永久保存'}</li>
                </ol>
              </div>
            </div>
          </Card>

          <Card title={t('uiTheme.switcher.aiGeneration') || 'AI主题生成'} style={{ marginTop: 16 }}>
            <div className="ui-theme-switcher__ai-generation">
              <p>{t('uiTheme.switcher.aiGenerationDesc') || '未来功能：使用AI生成独特的UI主题'}</p>
              <Button disabled>
                {t('uiTheme.switcher.comingSoon') || '敬请期待'}
              </Button>
            </div>
          </Card>
        </div>
      ),
    },
  ];

  return (
    <Modal
      title={
        <div className="ui-theme-switcher__title">
          <AppstoreOutlined /> {t('uiTheme.switcher.title') || 'UI主题切换器'}
        </div>
      }
      open={open}
      onCancel={onClose}
      width={1000}
      footer={null}
      className="ui-theme-switcher"
    >
      <Tabs defaultActiveKey="builtin" items={tabItems} />
    </Modal>
  );
}

export default UIThemeSwitcher;

