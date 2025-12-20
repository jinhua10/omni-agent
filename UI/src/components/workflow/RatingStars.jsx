import React from 'react';
import '../../assets/css/workflow/rating-stars.css';

const RatingStars = ({ rating = 0, size = 'medium', interactive = false, onRate }) => {
  const maxStars = 5;
  const fullStars = Math.floor(rating);
  const hasHalfStar = rating % 1 >= 0.5;
  const emptyStars = maxStars - fullStars - (hasHalfStar ? 1 : 0);

  const handleClick = (index) => {
    if (interactive && onRate) {
      onRate(index + 1);
    }
  };

  return (
    <div className={`rating-stars rating-stars-${size}`}>
      {/* 满星 */}
      {Array.from({ length: fullStars }).map((_, index) => (
        <span
          key={`full-${index}`}
          className={`star full ${interactive ? 'interactive' : ''}`}
          onClick={() => handleClick(index)}
        >
          ⭐
        </span>
      ))}

      {/* 半星 */}
      {hasHalfStar && (
        <span className="star half">
          ⭐
        </span>
      )}

      {/* 空星 */}
      {Array.from({ length: emptyStars }).map((_, index) => (
        <span
          key={`empty-${index}`}
          className={`star empty ${interactive ? 'interactive' : ''}`}
          onClick={() => handleClick(fullStars + (hasHalfStar ? 1 : 0) + index)}
        >
          ☆
        </span>
      ))}

      {/* 评分数值 */}
      <span className="rating-value">
        {rating.toFixed(1)}
      </span>
    </div>
  );
};

export default RatingStars;

