package com.vz.backend.business.dto.document;

import java.util.Date;

import com.vz.backend.business.domain.documentInternal.DocumentInternal;
import com.vz.backend.business.service.docInternal.DocInternalService;
import com.vz.backend.core.common.BussinessCommon;
import com.vz.backend.core.config.DocumentStatusEnum;

import lombok.AllArgsConstructor;
import lombok.Getter;
import lombok.NoArgsConstructor;
import lombok.Setter;

@Getter
@Setter
@NoArgsConstructor
@AllArgsConstructor
public class DocInternalForListDto {
	private Long id;
	private String numberOrSign;
	private String preview;
	private Long createBy;
	private String userCreateName;
	private Date createDate;
	private Date docDate;
	private Date signDate;
	private Date approveDate;
	private DocumentStatusEnum docStatus;
	private boolean canRetake;
	private boolean canDocinternal;

	public String getDocStatusName() {
		return docStatus.getName();
	}
	
	public DocInternalForListDto(Long id, String numberOrSign, String preview, DocumentStatusEnum docStatus,
			Long createBy, String userCreateName, Date createDate, Date signDate, Date docDate, Boolean read) {
		super();
		this.id = id;
		this.numberOrSign = numberOrSign;
		this.preview = preview;
		this.docStatus = docStatus;
		this.createBy = createBy;
		this.userCreateName = userCreateName;
		this.createDate = createDate;
		this.signDate = signDate;
		this.docDate = docDate;
		this.canRetake = BussinessCommon.getUserId().equals(createBy) && !Boolean.TRUE.equals(read)
				&& !DocInternalService.NOT_RETAKE_STATUS.contains(docStatus);
	}
}
