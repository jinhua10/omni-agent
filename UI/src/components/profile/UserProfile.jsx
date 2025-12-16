import React, { useState, useEffect } from 'react';
import { Card, Tabs, Avatar, Button, App } from 'antd';
import { UserOutlined, EditOutlined, SettingOutlined } from '@ant-design/icons';
import { useLanguage } from '../../contexts/LanguageContext';
import { profileApi } from '../../api/modules/profile';
import ProfileEditor from './ProfileEditor';
import UsageStatistics from './UsageStatistics';
import ContributionStats from './ContributionStats';
import AchievementPanel from './AchievementPanel';
import UserSettings from './UserSettings';
import '../../assets/css/profile/user-profile.css';

const UserProfile = () => {
  const { t } = useLanguage();
  const { message } = App.useApp();
  const [userInfo, setUserInfo] = useState(null);
  const [loading, setLoading] = useState(false);
  const [editModalVisible, setEditModalVisible] = useState(false);
  const [activeTab, setActiveTab] = useState('overview');

  // 加载用户信息
  const loadUserInfo = async () => {
    setLoading(true);
    try {
      const response = await profileApi.getUserInfo();
      // Backend returns UserInfo object directly
      const userData = response.data || response;
      setUserInfo(userData);
    } catch (error) {
      // 如果是404错误（接口未实现），使用默认用户数据
      if (error.response?.status === 404) {
        console.warn('⚠️ Profile info endpoint not implemented, using default user');
        setUserInfo({
          userId: 'guest',
          username: 'Guest User',
          email: 'guest@example.com',
          avatar: null,
          bio: 'Welcome to OmniAgent',
          joinDate: new Date().toISOString(),
        });
        return;
      }
      console.error('Failed to load user info:', error);
      message.error(t('profile.loadFailed'));
      setUserInfo(null);
    } finally {
      setLoading(false);
    }
  };

  useEffect(() => {
    loadUserInfo();
  }, []);

  if (!userInfo && !loading) {
    return null;
  }

  return (
    <div className="user-profile">
      {/* 用户信息卡片 */}
      <Card className="user-profile__card">
        <div className="user-profile__header">
          <div className="user-profile__avatar-section">
            <Avatar
              size={120}
              src={userInfo?.avatar}
              icon={<UserOutlined />}
              className="user-profile__avatar"
            />
            <Button
              type="primary"
              icon={<EditOutlined />}
              onClick={() => setEditModalVisible(true)}
              className="user-profile__edit-btn"
            >
              {t('profile.editInfo')}
            </Button>
          </div>

          <div className="user-profile__info">
            <h2 className="user-profile__name">
              {userInfo?.nickname || t('profile.defaultName')}
            </h2>
            <p className="user-profile__email">{userInfo?.email}</p>
            <p className="user-profile__bio">
              {userInfo?.bio || t('profile.noBio')}
            </p>

            {/* 快速统计 */}
            <div className="user-profile__stats">
              <div className="user-profile__stat-item">
                <span className="user-profile__stat-value">
                  {userInfo?.statistics?.qaCount || 0}
                </span>
                <span className="user-profile__stat-label">
                  {t('profile.qaCount')}
                </span>
              </div>
              <div className="user-profile__stat-item">
                <span className="user-profile__stat-value">
                  {userInfo?.statistics?.documentCount || 0}
                </span>
                <span className="user-profile__stat-label">
                  {t('profile.documentCount')}
                </span>
              </div>
              <div className="user-profile__stat-item">
                <span className="user-profile__stat-value">
                  {userInfo?.statistics?.contributionScore || 0}
                </span>
                <span className="user-profile__stat-label">
                  {t('profile.contributionScore')}
                </span>
              </div>
            </div>
          </div>
        </div>
      </Card>

      {/* 详细信息标签页 */}
      <Card className="user-profile__tabs-card">
        <Tabs 
          activeKey={activeTab} 
          onChange={setActiveTab}
          items={[
            {
              key: 'statistics',
              label: (
                <span>
                  <UserOutlined />
                  {t('profile.statistics')}
                </span>
              ),
              children: <UsageStatistics userId={userInfo?.id} />
            },
            {
              key: 'contribution',
              label: (
                <span>
                  <UserOutlined />
                  {t('profile.contribution')}
                </span>
              ),
              children: <ContributionStats userId={userInfo?.id} />
            },
            {
              key: 'achievement',
              label: (
                <span>
                  <UserOutlined />
                  {t('profile.achievement')}
                </span>
              ),
              children: <AchievementPanel userId={userInfo?.id} />
            },
            {
              key: 'settings',
              label: (
                <span>
                  <SettingOutlined />
                  {t('profile.settings')}
                </span>
              ),
              children: <UserSettings />
            }
          ]}
        />
      </Card>

      {/* 编辑个人信息模态框 */}
      <ProfileEditor
        visible={editModalVisible}
        userInfo={userInfo}
        onClose={() => setEditModalVisible(false)}
        onSuccess={() => {
          setEditModalVisible(false);
          loadUserInfo();
        }}
      />
    </div>
  );
};

export default UserProfile;

