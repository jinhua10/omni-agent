/**
 * Modal 模态框组件 (Modal Dialog Component)
 *
 * 封装 Ant Design Modal，提供统一的模态框
 * (Wraps Ant Design Modal for consistent dialogs)
 *
 * @author AI Reviewer Team
 * @since 2025-12-12
 */

import React from 'react'
import { Modal as AntModal } from 'antd'
import { useLanguage } from '@contexts/LanguageContext'
import PropTypes from 'prop-types'

/**
 * Modal 组件
 *
 * @param {Object} props - 组件属性
 * @param {boolean} props.visible - 是否显示
 * @param {string} props.title - 标题
 * @param {ReactNode} props.children - 内容
 * @param {Function} props.onOk - 确认回调
 * @param {Function} props.onCancel - 取消回调
 * @param {string} props.okText - 确认按钮文本
 * @param {string} props.cancelText - 取消按钮文本
 * @param {boolean} props.confirmLoading - 确认按钮加载状态
 * @param {string} props.width - 宽度
 * @param {boolean} props.centered - 是否居中显示
 * @param {boolean} props.maskClosable - 点击遮罩是否关闭
 *
 * @example
 * <Modal
 *   visible={visible}
 *   title="确认删除"
 *   onOk={handleOk}
 *   onCancel={handleCancel}
 * >
 *   <p>确定要删除这条记录吗？</p>
 * </Modal>
 */
function Modal({
  visible,
  title,
  children,
  onOk,
  onCancel,
  okText,
  cancelText,
  confirmLoading = false,
  width = 520,
  centered = true,
  maskClosable = true,
  ...restProps
}) {
  const { t } = useLanguage()

  return (
    <AntModal
      open={visible}
      title={title}
      onOk={onOk}
      onCancel={onCancel}
      okText={okText || t('common.confirm')}
      cancelText={cancelText || t('common.cancel')}
      confirmLoading={confirmLoading}
      width={width}
      centered={centered}
      maskClosable={maskClosable}
      className="app-modal"
      {...restProps}
    >
      {children}
    </AntModal>
  )
}

Modal.propTypes = {
  visible: PropTypes.bool.isRequired,
  title: PropTypes.string,
  children: PropTypes.node,
  onOk: PropTypes.func,
  onCancel: PropTypes.func,
  okText: PropTypes.string,
  cancelText: PropTypes.string,
  confirmLoading: PropTypes.bool,
  width: PropTypes.oneOfType([PropTypes.number, PropTypes.string]),
  centered: PropTypes.bool,
  maskClosable: PropTypes.bool,
}

/**
 * 确认对话框 (Confirm dialog)
 */
Modal.confirm = (config) => {
  AntModal.confirm({
    centered: true,
    className: 'app-modal',
    ...config,
  })
}

/**
 * 信息对话框 (Info dialog)
 */
Modal.info = (config) => {
  AntModal.info({
    centered: true,
    className: 'app-modal',
    ...config,
  })
}

/**
 * 成功对话框 (Success dialog)
 */
Modal.success = (config) => {
  AntModal.success({
    centered: true,
    className: 'app-modal',
    ...config,
  })
}

/**
 * 错误对话框 (Error dialog)
 */
Modal.error = (config) => {
  AntModal.error({
    centered: true,
    className: 'app-modal',
    ...config,
  })
}

/**
 * 警告对话框 (Warning dialog)
 */
Modal.warning = (config) => {
  AntModal.warning({
    centered: true,
    className: 'app-modal',
    ...config,
  })
}

export default Modal

