package com.vz.backend.business.dto;

import java.io.Serializable;
import java.util.Date;

import com.vz.backend.business.domain.Documents;

import lombok.AllArgsConstructor;
import lombok.Data;
import lombok.NoArgsConstructor;

@NoArgsConstructor
@Data
@AllArgsConstructor
public class DocumentReplyDto implements Serializable {

	/**
	 * 
	 */
	private static final long serialVersionUID = 1L;

	private int no;
	private Long id;
	private Long numberArrival;
	private String numberOrSign;
	private Date dateArrival;
	private Date dateIssued;
	private String preview;
	private String orgIssuedName;
	private Date deadline;

	public DocumentReplyDto(Long numberArrival, String numberOrSign, Date dateArrival, Date dateIssued, String preview,
			String orgIssuedName, Date deadline, Long docId) {
		this.numberArrival = numberArrival;
		this.numberOrSign = numberOrSign;
		this.dateArrival = dateArrival;
		this.dateIssued = dateIssued;
		this.preview = preview;
		this.deadline = deadline;
		this.id = docId;
		this.orgIssuedName = orgIssuedName;
	}

	public DocumentReplyDto(Documents d) {
		this.setId(d.getId());
		this.setDateArrival(d.getDateArrival());
		this.setDateIssued(d.getDateIssued());
		this.setDeadline(d.getDeadline());
		this.setPreview(d.getPreview());
		this.setNumberOrSign(d.getNumberOrSign());
		this.setNumberArrival(d.getNumberArrival());
		this.setOrgIssuedName(d.getPlaceSend());
	}
}
