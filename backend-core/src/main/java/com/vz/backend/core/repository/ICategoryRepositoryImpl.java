package com.vz.backend.core.repository;

import com.vz.backend.core.domain.Category;
import org.springframework.stereotype.Repository;

import javax.persistence.EntityManager;
import javax.persistence.PersistenceContext;
import javax.persistence.Query;
import java.util.List;

@Repository
public class ICategoryRepositoryImpl implements ICategoryRepositoryCustom {
	@PersistenceContext
	EntityManager entityManager;

	@Override
	public void updateOrdersAfterCurrentOrder(Integer orderNumber, Long categoryId, Long categoryTypeId) {
		List<Category> listCategory = this.getNeedUpdateList(orderNumber, categoryId, categoryTypeId);
		this.updateOrders(listCategory, orderNumber);
	}

//	methods use only in this class
	private void updateOrders(List<Category> categoryList, Integer currentOrderNumber) {

		if (categoryList.isEmpty()) {
			return;
		}

		String sqlQueryString = this.getUpdateOrdersString(categoryList, currentOrderNumber);

		Query query = entityManager.createNativeQuery(sqlQueryString);

		query.executeUpdate();
	}

	private List<Category> getNeedUpdateList(Integer orderNumber, Long categoryId, Long categoryTypeId) {
		Query query = entityManager.createNativeQuery(
				"SELECT *" + " FROM vz.sys_category" + " WHERE category_type_id = ?" + " AND id != ?"
						+ " AND (order_number >= ? OR order_number IS NULL)" + " ORDER BY order_number",
				Category.class);
		query.setParameter(1, categoryTypeId);
		query.setParameter(2, categoryId);
		query.setParameter(3, orderNumber);
		List<Category> listCategory = query.getResultList();

		return listCategory;
	}

	private String getUpdateOrdersString(List<Category> categoryList, Integer currentOrderNumber) {
		currentOrderNumber += 1;

		StringBuilder sqlQueryStringBuilder = new StringBuilder("UPDATE vz.sys_category SET order_number = CASE id ");
		StringBuilder queryConditionStringBuilder = new StringBuilder(" WHERE id IN(");
		for (Category category : categoryList) {
			sqlQueryStringBuilder.append(" WHEN ").append(category.getId()).append(" THEN ").append(currentOrderNumber);
			queryConditionStringBuilder.append(category.getId()).append(",");
			currentOrderNumber += 1;
		}
		sqlQueryStringBuilder.append(" END");

		String queryConditionString = queryConditionStringBuilder.substring(0, queryConditionStringBuilder.length() - 1) + ");";
		sqlQueryStringBuilder.append(queryConditionString);

		return sqlQueryStringBuilder.toString();
	}
}
