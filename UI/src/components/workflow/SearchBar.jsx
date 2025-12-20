import React, { useState } from 'react';
import { useLanguage } from '../../contexts/LanguageContext';
import '../../assets/css/workflow/search-bar.css';

const SearchBar = ({ onSearch }) => {
  const { t } = useLanguage();
  const [keyword, setKeyword] = useState('');

  const handleSearch = () => {
    onSearch(keyword);
  };

  const handleKeyPress = (e) => {
    if (e.key === 'Enter') {
      handleSearch();
    }
  };

  const handleClear = () => {
    setKeyword('');
    onSearch('');
  };

  return (
    <div className="search-bar">
      <div className="search-input-wrapper">
        <span className="search-icon">🔍</span>
        <input
          type="text"
          className="search-input"
          placeholder={t('workflowMarket.search.placeholder')}
          value={keyword}
          onChange={(e) => setKeyword(e.target.value)}
          onKeyPress={handleKeyPress}
        />
        {keyword && (
          <button className="clear-btn" onClick={handleClear}>
            ✕
          </button>
        )}
      </div>
      <button className="search-btn" onClick={handleSearch}>
        {t('workflowMarket.search.button')}
      </button>
    </div>
  );
};

export default SearchBar;

