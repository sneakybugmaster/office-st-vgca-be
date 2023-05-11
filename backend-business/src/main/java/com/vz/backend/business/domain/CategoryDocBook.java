package com.vz.backend.business.domain;

import javax.persistence.Column;
import javax.persistence.Entity;
import javax.persistence.Table;
import javax.persistence.UniqueConstraint;

import com.vz.backend.core.domain.BaseModel;

import lombok.Getter;
import lombok.NoArgsConstructor;
import lombok.Setter;

@Entity
@Table(name = "CATEGORY_DOC_BOOK", schema = "vz", uniqueConstraints = {
		@UniqueConstraint(columnNames = { "book_id", "category_id" }) })
@Getter
@Setter
@NoArgsConstructor
public class CategoryDocBook extends BaseModel {
	private static final long serialVersionUID = 1L;
	@Column(name = "book_id")
	private Long bookId;
	
	@Column(name = "category_id")
	private Long categoryId;
	
	public CategoryDocBook(Long bookId, Long categoryId) {
		super();
		this.bookId = bookId;
		this.categoryId = categoryId;
	}
}
