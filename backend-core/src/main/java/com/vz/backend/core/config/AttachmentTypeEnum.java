package com.vz.backend.core.config;

public enum AttachmentTypeEnum {
	DRAFT("draft"), 
	RELATE("relate"), 
	ENCRYPT("encrypt"),
	COMMENT("comment"),
	//Văn bản nội bộ
	VAN_BAN("File văn bản"),
	DA_KY("File đã ký"),
	PHU_LUC("File phụ lục"),
	BINH_LUAN("Bình luận nội bộ"),
	
	//for document in
	RESOLVED_FILE("Phiếu giải quyết văn bản đến")
	
	;

	private final String name;

	AttachmentTypeEnum(String name) {
		this.name = name;
	}

	public String getName() {
		return name;
	}
}
