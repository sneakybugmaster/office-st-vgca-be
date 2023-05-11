package com.vz.backend.core.dto;

import lombok.AllArgsConstructor;
import lombok.Data;
import lombok.NoArgsConstructor;

@Data
@AllArgsConstructor
@NoArgsConstructor
public class ReportDocumentDto {
	private Long notYet; // Chưa hoàn thành.
	private Long processed; // Chờ xử lý.
	private Long doneOrReceive; // Hoàn thành.
	private Long overDue; // Quá hạn.
	private Long returnDoc; // Trả lại.
	private Long choXLC; // Chờ XL chính.
	private Long choXLPH; // Chờ XL phối hợp.
	private Long choNDB; // Chờ nhận để biết.
	private Long choCD; // Chờ chỉ đạo.
	private Long choCYK; // Chờ cho ý kiến.
}
