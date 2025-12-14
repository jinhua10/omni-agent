import React, { createContext, useContext, useState, useEffect } from 'react';

const ThemeContext = createContext();

// 预定义主题配置 / Predefined theme configurations
const PRESET_THEMES = {
  light: {
    name: '浅色 / Light',
    primary: '#1890ff',
    background: '#ffffff',
    surface: '#f5f5f5',
    text: '#333333',
    textSecondary: '#666666',
    border: '#d9d9d9',
  },
  dark: {
    name: '暗色 / Dark',
    primary: '#597ef7',
    background: '#141414',
    surface: '#1f1f1f',
    text: '#e8e8e8',
    textSecondary: '#b8b8b8',
    border: '#434343',
  },
  blue: {
    name: '蓝色 / Blue',
    primary: '#1890ff',
    background: '#e6f7ff',
    surface: '#bae7ff',
    text: '#002766',
    textSecondary: '#0050b3',
    border: '#91d5ff',
  },
  green: {
    name: '绿色 / Green',
    primary: '#52c41a',
    background: '#f6ffed',
    surface: '#d9f7be',
    text: '#135200',
    textSecondary: '#389e0d',
    border: '#b7eb8f',
  },
  purple: {
    name: '紫色 / Purple',
    primary: '#722ed1',
    background: '#f9f0ff',
    surface: '#efdbff',
    text: '#22075e',
    textSecondary: '#531dab',
    border: '#d3adf7',
  },
  custom: {
    name: '自定义 / Custom',
    primary: '#1890ff',
    background: '#ffffff',
    surface: '#f5f5f5',
    text: '#333333',
    textSecondary: '#666666',
    border: '#d9d9d9',
  },
};

export const useTheme = () => {
  const context = useContext(ThemeContext);
  if (!context) {
    throw new Error('useTheme must be used within ThemeProvider');
  }
  return context;
};

export const ThemeProvider = ({ children }) => {
  // 当前主题名称 / Current theme name
  const [themeName, setThemeName] = useState(() => {
    return localStorage.getItem('themeName') || 'light';
  });

  // 自定义主题配置 / Custom theme configuration
  const [customTheme, setCustomTheme] = useState(() => {
    const saved = localStorage.getItem('customTheme');
    return saved ? JSON.parse(saved) : PRESET_THEMES.custom;
  });

  // 获取当前主题配置 / Get current theme configuration
  const getCurrentTheme = () => {
    return themeName === 'custom' ? customTheme : PRESET_THEMES[themeName];
  };

  // 应用主题到CSS变量 / Apply theme to CSS variables
  const applyTheme = (themeConfig) => {
    const root = document.documentElement;
    root.style.setProperty('--theme-primary', themeConfig.primary);
    root.style.setProperty('--theme-background', themeConfig.background);
    root.style.setProperty('--theme-surface', themeConfig.surface);
    root.style.setProperty('--theme-text', themeConfig.text);
    root.style.setProperty('--theme-text-secondary', themeConfig.textSecondary);
    root.style.setProperty('--theme-border', themeConfig.border);

    // 设置data-theme属性用于CSS选择器 / Set data-theme for CSS selectors
    root.setAttribute('data-theme', themeName);
  };

  useEffect(() => {
    const themeConfig = getCurrentTheme();
    applyTheme(themeConfig);
    localStorage.setItem('themeName', themeName);
  }, [themeName, customTheme]);

  // 切换到预设主题 / Switch to preset theme
  const setTheme = (name) => {
    if (PRESET_THEMES[name]) {
      setThemeName(name);
    }
  };

  // 切换深浅色模式 / Toggle light/dark mode
  const toggleTheme = () => {
    setThemeName(prev => prev === 'light' ? 'dark' : 'light');
  };

  // 更新自定义主题 / Update custom theme
  const updateCustomTheme = (updates) => {
    const newCustomTheme = { ...customTheme, ...updates };
    setCustomTheme(newCustomTheme);
    localStorage.setItem('customTheme', JSON.stringify(newCustomTheme));
    if (themeName === 'custom') {
      applyTheme(newCustomTheme);
    }
  };

  const value = {
    themeName,
    theme: getCurrentTheme(),
    presetThemes: PRESET_THEMES,
    setTheme,
    toggleTheme,
    updateCustomTheme,
  };

  return (
    <ThemeContext.Provider value={value}>
      {children}
    </ThemeContext.Provider>
  );
};

