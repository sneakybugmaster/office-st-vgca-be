package com.vz.backend.core.config;

public enum SortBy {
	
	// default
	UPDATEDATE("updateDate"), // Ngày cập nhật
	CREATEDATE("createDate"), // Ngày tạo

	// formula
	NAME("name"), // tên công thức - tên KPI - Tên bộ KPI
	TYPE("type"), // loại công thức

	// for KPI
	CODE("code"), // mã KPI
	MEASURE("measures.name"), // đơn vị đo
	MODE_MEASURE("modeMeasure"), // cách thức đo
	FREQUENCY("frequency.name"), // tần suất đo
	TYPE_OBJ("typeObj.name"), // loại đối tượng
	;
	
	public String field;

	private SortBy(String field) {
		this.field = field;
	}
}
