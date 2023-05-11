package com.vz.backend.business.dto.document;

import java.util.Date;
import java.util.List;

import com.vz.backend.business.domain.documentInternal.DocInternalReceiver;
import com.vz.backend.core.config.DocumentStatusEnum;

import lombok.Getter;
import lombok.NoArgsConstructor;
import lombok.Setter;

@Getter
@Setter
@NoArgsConstructor
public class DocInternalCreateDto {
	private Long docBookId;
	private Date docDate;
	private Date signDate;
	private Long docTypeId;
	private Long orgCreateId;
	private String numberOrSign;
	private String preview;
	private Long signerId;
	private Long urgentId;
	private Long securityId;
	private Integer issuedQuantity;
	private DocumentStatusEnum status;
	private List<Long> listUserApprove;
	private List<Long> listSigner;
	private List<Long> listOrgApprove;
	private List<Long> listCommenterApprove;
	private List<DocInternalReceiver> listReceiver;
}
