/**
 * Pagination 分页组件 (Pagination Component)
 *
 * 封装 Ant Design Pagination，提供统一的分页控件
 * (Wraps Ant Design Pagination for consistent pagination)
 *
 * @author AI Reviewer Team
 * @since 2025-12-12
 */

import React from 'react'
import { Pagination as AntPagination } from 'antd'
import PropTypes from 'prop-types'

/**
 * Pagination 组件
 *
 * @param {Object} props - 组件属性
 * @param {number} props.current - 当前页码
 * @param {number} props.total - 数据总数
 * @param {number} props.pageSize - 每页条数
 * @param {Function} props.onChange - 页码改变回调
 * @param {Function} props.onShowSizeChange - 每页条数改变回调
 * @param {Array} props.pageSizeOptions - 每页条数选项
 * @param {boolean} props.showSizeChanger - 是否显示每页条数选择器
 * @param {boolean} props.showQuickJumper - 是否显示快速跳转
 * @param {boolean} props.showTotal - 是否显示总数
 * @param {string} props.size - 尺寸 (default/small)
 *
 * @example
 * <Pagination
 *   current={1}
 *   total={100}
 *   pageSize={10}
 *   onChange={handlePageChange}
 * />
 */
function Pagination({
  current = 1,
  total = 0,
  pageSize = 10,
  onChange,
  onShowSizeChange,
  pageSizeOptions = ['10', '20', '50', '100'],
  showSizeChanger = true,
  showQuickJumper = true,
  showTotal = true,
  size = 'default',
  ...restProps
}) {
  // 显示总数的函数 (Function to show total)
  const showTotalText = (total, range) => {
    return `${range[0]}-${range[1]} / ${total}`
  }

  return (
    <div className="app-pagination">
      <AntPagination
        current={current}
        total={total}
        pageSize={pageSize}
        onChange={onChange}
        onShowSizeChange={onShowSizeChange}
        pageSizeOptions={pageSizeOptions}
        showSizeChanger={showSizeChanger}
        showQuickJumper={showQuickJumper}
        showTotal={showTotal ? showTotalText : undefined}
        size={size}
        {...restProps}
      />
    </div>
  )
}

Pagination.propTypes = {
  current: PropTypes.number,
  total: PropTypes.number,
  pageSize: PropTypes.number,
  onChange: PropTypes.func,
  onShowSizeChange: PropTypes.func,
  pageSizeOptions: PropTypes.array,
  showSizeChanger: PropTypes.bool,
  showQuickJumper: PropTypes.bool,
  showTotal: PropTypes.bool,
  size: PropTypes.oneOf(['default', 'small']),
}

export default Pagination

