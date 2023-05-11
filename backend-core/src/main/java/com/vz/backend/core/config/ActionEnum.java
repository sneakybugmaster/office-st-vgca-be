package com.vz.backend.core.config;

public enum ActionEnum {
	ADD("add"), PRE_ADD("pre-add"), UPDATE("update"), PRE_UPDATE("pre-update"), DONE("done"), REJECT("reject"),
	ACCEPT("accept"), PRE_DONE("pre-done"), DELETE("delete"), LOGIN("login"), REGISTRY("register"),
	EXPORTEXCEL("exportExcel"), EXPORTPDF("exportPdf"), TRANSFER_HANDLE("transfer_handle"), RETURN_DOC("return_doc"),
	COMMING_TO("coming_to"), COMMENT("comment"), READ("read"), ISSUED("issued");

	private final String name;

	ActionEnum(String name) {
		this.name = name;
	}

	public String getName() {
		return name;
	}
}
