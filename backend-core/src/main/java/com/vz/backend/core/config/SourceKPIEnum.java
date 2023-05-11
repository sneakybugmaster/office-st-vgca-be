package com.vz.backend.core.config;

import java.util.ArrayList;
import java.util.List;

import com.vz.backend.core.dto.LabelValueDto;

public enum SourceKPIEnum {
	DI_TOTAL_DOCUMENT_IN_DONE_WITH_ALL_ROLE("Tổng số văn bản đến được hoàn thành"),
	DI_TOTAL_DOCUMENT_IN_DONE_WITH_MAIN_ROLE("Tổng số văn bản đến được hoàn thành với vai trò xử lý chính"),
	DI_TOTAL_DOCUMENT_IN_DONE_WITH_SUPPORT_ROLE("Tổng số văn bản đến được hoàn thành với vai trò phối hợp"),
	DI_TOTAL_DOCUMENT_IN_DONE_WITH_SHOW_ROLE("Tổng số văn bản đến được hoàn thành với vai trò nhận để biết"),
	DI_TOTAL_DOCUMENT_IN_DID_WITH_ALL_ROLE("Tổng số văn bản đến đã xử lý"),
	DI_TOTAL_DOCUMENT_IN_DID_WITH_MAIN_ROLE("Tổng số văn bản đến đã xử lý với vai trò xử lý chính"),
	DI_TOTAL_DOCUMENT_IN_DID_WITH_SUPPORT_ROLE("Tổng số văn bản đến đã xử lý với vai trò phối hợp"),
	DI_TOTAL_DOCUMENT_IN_DID_WITH_SHOW_ROLE("Tổng số văn bản đến đã xử lý với vai trò nhận để biết"),
	DI_TOTAL_DOCUMENT_IN_DONE_AFTER_DEADLINE("Tổng số văn bản đến được hoàn thành sau thời hạn xử lý"),
	DI_TOTAL_DOCUMENT_IN_DID_AFTER_DEADLINE("Tổng số văn bản đến xử lý sau thời hạn xử lý"),
	DI_TOTAL_DOCUMENT_IN_DONE_BEFORE_DEADLINE("Tổng số văn bản đến được hoàn thành trước thời hạn xử lý"),
	DI_TOTAL_DOCUMENT_IN_DID_BEFORE_DEADLINE("Tổng số văn bản đến xử lý trước thời hạn xử lý"),
	DO_TOTAL_DOCUMENT_OUT_DONE("Tổng số văn bản đi được hoàn thành"),
	DO_TOTAL_DOCUMENT_OUT_DID("Tổng số văn bản đi đã xử lý"),
	T_TOTAL_TASK_DONE_WITH_ALL_ROLE("Tổng số công việc được hoàn thành"),
	T_TOTAL_TASK_DONE_WITH_MAIN_ROLE("Tổng số công việc được hoàn thành với vai trò xử lý chính"),
	T_TOTAL_TASK_DONE_WITH_SUPPORT_ROLE("Tổng số công việc được hoàn thành với vai trò phối hợp"),
	T_TOTAL_TASK_DID_WITH_ALL_ROLE("Tổng số công việc được hoàn thành"),
	T_TOTAL_TASK_DID_WITH_MAIN_ROLE("Tổng số công việc được hoàn thành với vai trò xử lý chính"),
	T_TOTAL_TASK_DID_WITH_SUPPORT_ROLE("Tổng số công việc được hoàn thành với vai trò phối hợp"),
	T_TOTAL_TASK_DONE_AFTER_DEADLINE("Tổng số công việc được hoàn thành sau thời hạn xử lý"),
	T_TOTAL_TASK_DID_AFTER_DEADLINE("Tổng số công việc xử lý sau thời hạn xử lý"),
	T_TOTAL_TASK_DONE_BEFORE_DEADLINE("Tổng số công việc được hoàn thành trước thời hạn xử lý"),
	T_TOTAL_TASK_DID_BEFORE_DEADLINE("Tổng số công việc xử lý trước thời hạn xử lý"),
	;

	private String name;

	public String getName() {
		return this.name;
	}

	SourceKPIEnum(String name) {
		this.name = name;
	}
	
	public static List<LabelValueDto<String>> get() {
		List<LabelValueDto<String>> data = new ArrayList<>();
		for (SourceKPIEnum v : values()) {
			data.add(new LabelValueDto<>(v.toString(), v.name));
		}
		return data;
	}
	
}
