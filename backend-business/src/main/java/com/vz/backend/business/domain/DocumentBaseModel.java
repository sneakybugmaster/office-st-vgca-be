package com.vz.backend.business.domain;

import com.vz.backend.core.common.BussinessCommon;
import com.vz.backend.core.domain.BaseModel;
import com.vz.backend.core.domain.User;
import lombok.Data;

import javax.persistence.Column;
import javax.persistence.MappedSuperclass;
import java.util.Date;

@MappedSuperclass
@Data
public abstract class DocumentBaseModel extends BaseModel {
	@Column(name = "place_receive")
	private String placeReceive;

	@Column(name = "document_detail")
	private String documentDetail;

	@Column(name = "identifier") // mã định danh cơ quan
	private String identifier;

	@Column(name = "doc_code") // mã định danh văn bản
	private String docCode;

	@Column(name = "send_date") // thời gian gửi
	private Date sendDate;

	@Column(name = "receive_date") // thời gian nhận
	private Date receiveDate;

	@Column(name = "signer_name") // tên người ký
	private String signerName;
}
