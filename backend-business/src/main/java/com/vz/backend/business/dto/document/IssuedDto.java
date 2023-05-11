package com.vz.backend.business.dto.document;

import java.util.Date;
import java.util.List;

import com.vz.backend.business.domain.DocumentReceive;

import lombok.Getter;
import lombok.NoArgsConstructor;
import lombok.Setter;

@Getter
@Setter
@NoArgsConstructor
public class IssuedDto {
	private Long id;
	private Long bookId;
	private Long numberInBook;
	private String numberOrSign;
	private Date dateIssued;
	private List<DocumentReceive> listReceive;
	private String listSignersName;
}
