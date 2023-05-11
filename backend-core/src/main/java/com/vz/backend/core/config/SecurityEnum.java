package com.vz.backend.core.config;

public enum SecurityEnum {
	ORG("Chuyển cho Tổ chức"), CLERICAL("Chuyển cho Văn thư");
	
	private String name;
	
	SecurityEnum(String name) {
		this.name = name;
	}
	
}
