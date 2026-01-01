import React from 'react';
import { motion } from 'framer-motion';
import './AnimatedContainer.css';

/**
 * 动画容器组件 - 解决 Framer Motion 文字颜色问题
 *
 * 使用方式:
 * <AnimatedContainer
 *   initial={{ opacity: 0, y: 20 }}
 *   animate={{ opacity: 1, y: 0 }}
 *   transition={{ delay: 0.2 }}
 * >
 *   内容
 * </AnimatedContainer>
 */
const AnimatedContainer = ({
  children,
  className = '',
  initial = { opacity: 0 },
  animate = { opacity: 1 },
  transition = {},
  whileHover,
  whileTap,
  onClick,
  style = {},
  ...restProps
}) => {
  return (
    <motion.div
      className={`animated-container ${className}`}
      initial={initial}
      animate={animate}
      transition={transition}
      whileHover={whileHover}
      whileTap={whileTap}
      onClick={onClick}
      style={style}
      {...restProps}
    >
      {children}
    </motion.div>
  );
};

export default React.memo(AnimatedContainer);

