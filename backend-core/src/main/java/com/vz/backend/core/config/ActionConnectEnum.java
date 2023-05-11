package com.vz.backend.core.config;

public enum ActionConnectEnum {
	CONNECT("Kết nối"), SEND_DOC("Gửi văn bản đi"), RECEIVE_DOC("Nhận văn bản"), REQUEST_ORG("Nhận thông tin tổ chức"),
	REQUEST_USER_CERT("Nhận thông tin chứng thực người dùng"), REQUEST_ADD_ENCRYPT("Yêu cầu chia sẻ tệp mã hóa"),
	REJECT_RECEIVE_DOC("Từ chối nhận văn bản");

	private final String name;

	ActionConnectEnum(String name) {
		this.name = name;
	}

	public String getName() {
		return name;
	}
}
