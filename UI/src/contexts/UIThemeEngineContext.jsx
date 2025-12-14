/**
 * UI主题引擎上下文 / UI Theme Engine Context
 *
 * 实现功能与UI展示的完全解耦，支持动态切换主题
 * Implements complete separation of functionality and UI presentation, supports dynamic theme switching
 *
 * @author AI Reviewer Team
 * @since 2025-12-12
 */

import React, { createContext, useContext, useState, useEffect } from 'react';

const UIThemeEngineContext = createContext();

/**
 * UI主题定义 / UI Theme Definition
 * 每个主题包含完整的布局和组件定义
 * Each theme contains complete layout and component definitions
 */
export const UI_THEMES = {
  // 默认主题（当前现代化布局）/ Default theme (current modern layout)
  modern: {
    id: 'modern',
    name: {
      zh: '现代商务',
      en: 'Modern Business',
    },
    description: {
      zh: '简洁专业的现代化商务风格',
      en: 'Clean and professional modern business style',
    },
    preview: '/themes/modern-preview.png',
    type: 'builtin', // 内置主题 / Built-in theme
    version: '1.0.0',
    author: 'AI Reviewer Team',
    // 主题配置 / Theme configuration
    config: {
      layout: 'sidebar', // sidebar | topbar | mixed
      animation: 'smooth', // smooth | minimal | none
      density: 'comfortable', // compact | comfortable | spacious
    },
    // 页面UI壳子映射 / Page UI shell mapping
    shellMapping: {
      collaboration: () => import('../components/theme/shells/modern/CollaborationShell'),
      // 其他页面可以继续添加 / Other pages can be added
    },
    status: 'active',
  },

  // 气泡主题 / Bubble theme
  bubble: {
    id: 'bubble',
    name: {
      zh: '梦幻气泡',
      en: 'Dreamy Bubble',
    },
    description: {
      zh: '可爱梦幻的气泡风格界面',
      en: 'Cute and dreamy bubble-style interface',
    },
    preview: '/themes/bubble-preview.png',
    type: 'builtin',
    version: '1.0.0',
    author: 'AI Reviewer Team',
    config: {
      layout: 'floating',
      animation: 'bouncy',
      density: 'spacious',
    },
    // 页面UI壳子映射 / Page UI shell mapping
    shellMapping: {
      qa: () => import('../components/theme/shells/bubble/QAShell'),
      home: () => import('../components/theme/shells/bubble/HomeShell'),
      documents: () => import('../components/theme/shells/bubble/DocumentManagement'),
      collaboration: () => import('../components/theme/shells/bubble/CollaborationShell'),
      analytics: () => import('../components/theme/shells/bubble/AnalyticsShell'),
      settings: () => import('../components/theme/shells/bubble/SettingsShell'),
    },
    // 状态改为active / Status changed to active
    status: 'active',
  },

  // 动漫卡通主题（未来实现）/ Anime cartoon theme (future implementation)
  anime: {
    id: 'anime',
    name: {
      zh: '二次元动漫',
      en: 'Anime Style',
    },
    description: {
      zh: '充满活力的二次元动漫风格',
      en: 'Vibrant anime-style interface',
    },
    preview: '/themes/anime-preview.png',
    type: 'builtin',
    version: '1.0.0',
    author: 'AI Reviewer Team',
    config: {
      layout: 'card',
      animation: 'dynamic',
      density: 'comfortable',
    },
    status: 'developing',
  },

  // 赛博朋克主题（未来实现）/ Cyberpunk theme (future implementation)
  cyberpunk: {
    id: 'cyberpunk',
    name: {
      zh: '赛博朋克',
      en: 'Cyberpunk',
    },
    description: {
      zh: '未来科幻的赛博朋克风格',
      en: 'Futuristic cyberpunk style',
    },
    preview: '/themes/cyberpunk-preview.png',
    type: 'builtin',
    version: '1.0.0',
    author: 'AI Reviewer Team',
    config: {
      layout: 'grid',
      animation: 'glitch',
      density: 'compact',
    },
    status: 'developing',
  },

  // ========== 中国传统节日主题系列 / Chinese Traditional Festival Themes ==========

  // 春节主题（中国年味）/ Spring Festival theme (Chinese New Year)
  springFestival: {
    id: 'springFestival',
    name: {
      zh: '春节年味',
      en: 'Spring Festival',
    },
    description: {
      zh: '喜庆热闹的中国春节风格，红红火火过大年',
      en: 'Festive Chinese New Year style with joy and prosperity',
    },
    preview: '/themes/spring-festival-preview.png',
    type: 'builtin',
    version: '1.0.0',
    author: 'AI Reviewer Team',
    config: {
      layout: 'festive',
      animation: 'fireworks',
      density: 'comfortable',
      colors: {
        primary: '#D32F2F', // 中国红 / Chinese red
        secondary: '#FFD700', // 金色 / Gold
        accent: '#FF6B6B', // 喜庆红 / Festive red
      },
      decorations: ['lanterns', 'firecrackers', 'couplets'],
    },
    status: 'developing',
  },

  // 中秋主题 / Mid-Autumn Festival theme
  midAutumn: {
    id: 'midAutumn',
    name: {
      zh: '中秋团圆',
      en: 'Mid-Autumn Festival',
    },
    description: {
      zh: '温馨团圆的中秋节风格，明月寄相思',
      en: 'Warm Mid-Autumn Festival style with full moon',
    },
    preview: '/themes/mid-autumn-preview.png',
    type: 'builtin',
    version: '1.0.0',
    author: 'AI Reviewer Team',
    config: {
      layout: 'round',
      animation: 'moon-phases',
      density: 'comfortable',
      colors: {
        primary: '#FFA726', // 月光橙 / Moonlight orange
        secondary: '#1A237E', // 深蓝夜空 / Deep blue night
        accent: '#FFE082', // 淡黄 / Pale yellow
      },
      decorations: ['moon', 'mooncakes', 'rabbits'],
    },
    status: 'developing',
  },

  // 端午主题 / Dragon Boat Festival theme
  dragonBoat: {
    id: 'dragonBoat',
    name: {
      zh: '端午龙舟',
      en: 'Dragon Boat Festival',
    },
    description: {
      zh: '传统端午节风格，龙舟竞渡粽叶飘香',
      en: 'Traditional Dragon Boat Festival with racing dragons',
    },
    preview: '/themes/dragon-boat-preview.png',
    type: 'builtin',
    version: '1.0.0',
    author: 'AI Reviewer Team',
    config: {
      layout: 'flowing',
      animation: 'rowing',
      density: 'comfortable',
      colors: {
        primary: '#4CAF50', // 粽叶绿 / Bamboo leaf green
        secondary: '#FFC107', // 金黄 / Golden yellow
        accent: '#00ACC1', // 江水蓝 / River blue
      },
      decorations: ['dragon-boats', 'zongzi', 'herbs'],
    },
    status: 'developing',
  },

  // 清明主题 / Qingming Festival theme
  qingming: {
    id: 'qingming',
    name: {
      zh: '清明时节',
      en: 'Qingming Festival',
    },
    description: {
      zh: '清雅素净的清明节风格，缅怀追思',
      en: 'Elegant Qingming Festival with spring serenity',
    },
    preview: '/themes/qingming-preview.png',
    type: 'builtin',
    version: '1.0.0',
    author: 'AI Reviewer Team',
    config: {
      layout: 'serene',
      animation: 'willow-breeze',
      density: 'spacious',
      colors: {
        primary: '#9CCC65', // 春绿 / Spring green
        secondary: '#90A4AE', // 灰蓝 / Grey blue
        accent: '#E0E0E0', // 素净灰 / Plain grey
      },
      decorations: ['willows', 'flowers', 'paper-offerings'],
    },
    status: 'developing',
  },

  // 七夕主题 / Qixi Festival theme
  qixi: {
    id: 'qixi',
    name: {
      zh: '七夕情缘',
      en: 'Qixi Festival',
    },
    description: {
      zh: '浪漫温馨的七夕节风格，鹊桥相会',
      en: 'Romantic Qixi Festival with Magpie Bridge',
    },
    preview: '/themes/qixi-preview.png',
    type: 'builtin',
    version: '1.0.0',
    author: 'AI Reviewer Team',
    config: {
      layout: 'romantic',
      animation: 'stars-twinkling',
      density: 'comfortable',
      colors: {
        primary: '#E91E63', // 浪漫粉 / Romantic pink
        secondary: '#9C27B0', // 紫色 / Purple
        accent: '#FFD54F', // 星光金 / Starlight gold
      },
      decorations: ['magpies', 'stars', 'weaving-tools'],
    },
    status: 'developing',
  },

  // 元宵主题 / Lantern Festival theme
  lanternFestival: {
    id: 'lanternFestival',
    name: {
      zh: '元宵灯会',
      en: 'Lantern Festival',
    },
    description: {
      zh: '璀璨绚丽的元宵节风格，花灯齐放',
      en: 'Brilliant Lantern Festival with colorful lanterns',
    },
    preview: '/themes/lantern-festival-preview.png',
    type: 'builtin',
    version: '1.0.0',
    author: 'AI Reviewer Team',
    config: {
      layout: 'luminous',
      animation: 'lantern-glow',
      density: 'comfortable',
      colors: {
        primary: '#FF5722', // 灯笼红 / Lantern red
        secondary: '#FFC107', // 暖黄 / Warm yellow
        accent: '#FF9800', // 橙光 / Orange glow
      },
      decorations: ['lanterns', 'tangyuan', 'riddles'],
    },
    status: 'developing',
  },
};

/**
 * useUIThemeEngine Hook
 * 使用UI主题引擎 / Use UI theme engine
 */
export const useUIThemeEngine = () => {
  const context = useContext(UIThemeEngineContext);
  if (!context) {
    throw new Error('useUIThemeEngine must be used within UIThemeEngineProvider');
  }
  return context;
};

/**
 * UI主题引擎Provider / UI Theme Engine Provider
 */
export const UIThemeEngineProvider = ({ children }) => {
  // 错误状态 / Error state
  const [error, setError] = useState(null);

  // 当前UI主题 / Current UI theme
  const [currentUITheme, setCurrentUITheme] = useState(() => {
    try {
      const saved = localStorage.getItem('uiTheme');
      // 验证保存的主题是否存在 / Validate if saved theme exists
      if (saved && UI_THEMES[saved]) {
        return saved;
      }
      return 'modern'; // 默认主题 / Default theme
    } catch (error) {
      console.error('❌ Failed to load theme from localStorage:', error);
      return 'modern'; // 出错时使用默认主题 / Use default theme on error
    }
  });

  // 自定义主题列表 / Custom theme list
  const [customThemes, setCustomThemes] = useState(() => {
    try {
      const saved = localStorage.getItem('customThemes');
      if (saved) {
        const parsed = JSON.parse(saved);
        return Array.isArray(parsed) ? parsed : [];
      }
      return [];
    } catch (error) {
      console.error('❌ Failed to load custom themes from localStorage:', error);
      return [];
    }
  });

  // 主题加载状态 / Theme loading state
  const [themeLoading, setThemeLoading] = useState(false);

  // 获取所有可用主题 / Get all available themes
  const getAllThemes = () => {
    const builtinThemes = Object.values(UI_THEMES);
    return [...builtinThemes, ...customThemes];
  };

  // 获取当前主题配置 / Get current theme configuration
  const getCurrentThemeConfig = () => {
    try {
      const allThemes = getAllThemes();
      const theme = allThemes.find(theme => theme.id === currentUITheme);

      // 如果找不到当前主题，返回默认主题 / If current theme not found, return default theme
      if (!theme) {
        console.warn('⚠️ Current theme not found, using default theme:', currentUITheme);
        return UI_THEMES.modern;
      }

      return theme;
    } catch (error) {
      console.error('❌ Error getting theme config, using default:', error);
      return UI_THEMES.modern; // 出错时返回默认主题 / Return default theme on error
    }
  };

  // 切换UI主题 / Switch UI theme
  const switchUITheme = async (themeId) => {
    const theme = getAllThemes().find(t => t.id === themeId);

    if (!theme) {
      console.error('Theme not found:', themeId);
      return false;
    }

    // 检查主题状态 / Check theme status
    if (theme.status === 'developing') {
      console.warn('Theme is in development:', themeId);
      return false;
    }

    setThemeLoading(true);

    try {
      // 这里可以加载主题资源 / Load theme resources here
      // 例如：动态导入主题组件、样式等
      // For example: dynamically import theme components, styles, etc.

      await new Promise(resolve => setTimeout(resolve, 500)); // 模拟加载 / Simulate loading

      setCurrentUITheme(themeId);
      localStorage.setItem('uiTheme', themeId);

      console.log('✅ UI Theme switched to:', theme.name.zh);
      return true;
    } catch (error) {
      console.error('❌ Failed to switch UI theme:', error);
      return false;
    } finally {
      setThemeLoading(false);
    }
  };

  // 上传主题到服务器 / Upload theme to server
  const uploadThemeToServer = async (themeData, themeFiles = []) => {
    try {
      const formData = new FormData();

      // 添加主题配置 / Add theme configuration
      formData.append('themeConfig', JSON.stringify(themeData));

      // 添加主题文件（CSS、图片等）/ Add theme files (CSS, images, etc.)
      themeFiles.forEach((file, index) => {
        formData.append(`file_${index}`, file);
      });

      // 上传到服务器 / Upload to server
      const response = await fetch('/api/themes/upload', {
        method: 'POST',
        body: formData,
        // 添加超时控制 / Add timeout control
        signal: AbortSignal.timeout(10000), // 10秒超时 / 10 seconds timeout
      });

      if (!response.ok) {
        const errorText = await response.text().catch(() => 'Unknown error');
        throw new Error(`Server returned ${response.status}: ${errorText}`);
      }

      const result = await response.json();
      console.log('✅ Theme uploaded to server:', result);

      return {
        success: true,
        serverPath: result.path, // 服务器存储路径 / Server storage path
        themeId: result.themeId,
      };
    } catch (error) {
      console.error('❌ Failed to upload theme to server:', error);
      // 返回详细的错误信息 / Return detailed error information
      return {
        success: false,
        error: error.message || 'Unknown error occurred',
        fallbackToLocal: true, // 标记应该回退到本地存储 / Mark should fallback to local storage
      };
    }
  };

  // 从服务器加载主题 / Load theme from server
  const loadThemeFromServer = async (themeId) => {
    try {
      const response = await fetch(`/api/themes/${themeId}`, {
        // 添加超时控制 / Add timeout control
        signal: AbortSignal.timeout(5000), // 5秒超时 / 5 seconds timeout
      });

      if (!response.ok) {
        throw new Error(`Server returned ${response.status}`);
      }

      const themeData = await response.json();
      console.log('✅ Theme loaded from server:', themeData);
      return themeData;
    } catch (error) {
      console.warn('⚠️ Failed to load theme from server, using local fallback:', error.message);
      // 返回null，调用方应处理降级 / Return null, caller should handle degradation
      return null;
    }
  };

  // 安装自定义主题（支持服务器持久化）/ Install custom theme (with server persistence)
  const installCustomTheme = async (themeData, options = {}) => {
    try {
      // 验证主题数据 / Validate theme data
      if (!themeData.id || !themeData.name) {
        throw new Error('Invalid theme data: missing id or name');
      }

      // 检查是否已存在 / Check if already exists
      const exists = getAllThemes().some(t => t.id === themeData.id);
      if (exists) {
        throw new Error('Theme already exists: ' + themeData.id);
      }

      // 添加到自定义主题列表 / Add to custom theme list
      const newTheme = {
        ...themeData,
        type: 'custom',
        installDate: new Date().toISOString(),
        source: 'local', // 默认本地 / Default to local
      };

      // 如果选择服务器持久化 / If server persistence is chosen
      if (options.uploadToServer && options.themeFiles) {
        console.log('🔄 Attempting to upload theme to server...');

        try {
          const uploadResult = await uploadThemeToServer(newTheme, options.themeFiles);

          if (uploadResult.success) {
            newTheme.serverPath = uploadResult.serverPath;
            newTheme.source = 'server';
            console.log('✅ Theme persisted to server:', uploadResult.serverPath);
          } else {
            console.warn('⚠️ Server upload failed, saving to local storage only');
            console.warn('Error:', uploadResult.error);
            // 继续使用本地存储，不抛出错误 / Continue with local storage, don't throw error
          }
        } catch (serverError) {
          console.warn('⚠️ Server not available, saving to local storage only:', serverError.message);
          // 服务器不可用时，继续本地安装 / Continue with local installation when server unavailable
        }
      }

      // 无论服务器是否可用，都保存到本地 / Save to local regardless of server availability
      try {
        const updatedCustomThemes = [...customThemes, newTheme];
        setCustomThemes(updatedCustomThemes);
        localStorage.setItem('customThemes', JSON.stringify(updatedCustomThemes));

        const themeName = newTheme.name.zh || newTheme.name.en || newTheme.id;
        console.log('✅ Custom theme installed locally:', themeName);

        return {
          success: true,
          theme: newTheme,
          installedLocally: true,
          installedOnServer: newTheme.source === 'server',
        };
      } catch (localError) {
        console.error('❌ Failed to save theme to local storage:', localError);
        throw new Error('Failed to save theme locally: ' + localError.message);
      }
    } catch (error) {
      console.error('❌ Failed to install custom theme:', error);
      return {
        success: false,
        error: error.message || 'Unknown error occurred',
      };
    }
  };

  // 从服务器同步主题列表 / Sync theme list from server
  const syncThemesFromServer = async () => {
    try {
      const response = await fetch('/api/themes/list', {
        // 添加超时控制 / Add timeout control
        signal: AbortSignal.timeout(5000), // 5秒超时 / 5 seconds timeout
      });

      if (!response.ok) {
        throw new Error(`Server returned ${response.status}`);
      }

      const serverThemes = await response.json();

      // 验证返回的数据 / Validate returned data
      if (!Array.isArray(serverThemes)) {
        console.warn('⚠️ Server returned invalid theme list, using local themes');
        return false;
      }

      // 合并服务器主题和本地主题 / Merge server themes with local themes
      const mergedThemes = [...customThemes];

      serverThemes.forEach(serverTheme => {
        const exists = mergedThemes.some(t => t.id === serverTheme.id);
        if (!exists) {
          mergedThemes.push({
            ...serverTheme,
            source: 'server',
          });
        }
      });

      setCustomThemes(mergedThemes);
      localStorage.setItem('customThemes', JSON.stringify(mergedThemes));

      console.log('✅ Themes synced from server:', mergedThemes.length);
      return true;
    } catch (error) {
      console.warn('⚠️ Failed to sync themes from server, using local themes:', error.message);
      // 即使同步失败，也保持本地主题可用 / Keep local themes available even if sync fails
      return false;
    }
  };

  // 卸载自定义主题 / Uninstall custom theme
  const uninstallCustomTheme = (themeId) => {
    try {
      const updatedCustomThemes = customThemes.filter(t => t.id !== themeId);
      setCustomThemes(updatedCustomThemes);
      localStorage.setItem('customThemes', JSON.stringify(updatedCustomThemes));

      // 如果当前主题被卸载，切换到默认主题 / If current theme is uninstalled, switch to default
      if (currentUITheme === themeId) {
        switchUITheme('modern');
      }

      console.log('✅ Custom theme uninstalled:', themeId);
      return true;
    } catch (error) {
      console.error('❌ Failed to uninstall custom theme:', error);
      return false;
    }
  };

  // 导出主题配置 / Export theme configuration
  const exportThemeConfig = (themeId) => {
    const theme = getAllThemes().find(t => t.id === themeId);
    if (!theme) return null;

    const exportData = {
      ...theme,
      exportDate: new Date().toISOString(),
      version: '1.0.0',
    };

    return JSON.stringify(exportData, null, 2);
  };

  // 导入主题配置 / Import theme configuration
  const importThemeConfig = async (configJson) => {
    try {
      const themeData = JSON.parse(configJson);
      return await installCustomTheme(themeData);
    } catch (error) {
      console.error('❌ Failed to import theme:', error);
      return false;
    }
  };

  // 初始化和错误恢复 / Initialization and error recovery
  useEffect(() => {
    // 验证当前主题是否有效 / Validate if current theme is valid
    const validateTheme = () => {
      try {
        const config = getCurrentThemeConfig();
        if (!config || config.id !== currentUITheme) {
          console.warn('⚠️ Invalid theme detected, resetting to default');
          setCurrentUITheme('modern');
          localStorage.setItem('uiTheme', 'modern');
        }
      } catch (error) {
        console.error('❌ Error validating theme, resetting to default:', error);
        setCurrentUITheme('modern');
        localStorage.setItem('uiTheme', 'modern');
        setError(error);
      }
    };

    validateTheme();

    // 尝试从服务器同步主题（静默失败）/ Try to sync themes from server (silent failure)
    syncThemesFromServer().catch(err => {
      console.log('ℹ️ Server themes not available, using local themes only');
    });
  }, []); // eslint-disable-line

  const value = {
    // 状态 / State
    currentUITheme,
    currentThemeConfig: getCurrentThemeConfig(),
    themeLoading,
    error, // 暴露错误状态 / Expose error state

    // 主题列表 / Theme lists
    builtinThemes: Object.values(UI_THEMES),
    customThemes,
    allThemes: getAllThemes(),

    // 操作方法 / Operation methods
    switchUITheme,
    installCustomTheme,
    uninstallCustomTheme,
    exportThemeConfig,
    importThemeConfig,

    // 服务器持久化方法 / Server persistence methods
    uploadThemeToServer,
    loadThemeFromServer,
    syncThemesFromServer,
  };

  // 如果有严重错误，显示错误提示但不阻止渲染 / If critical error, show error but don't block rendering
  if (error) {
    console.error('⚠️ Theme engine error (using fallback):', error);
  }

  return (
    <UIThemeEngineContext.Provider value={value}>
      {children}
    </UIThemeEngineContext.Provider>
  );
};

