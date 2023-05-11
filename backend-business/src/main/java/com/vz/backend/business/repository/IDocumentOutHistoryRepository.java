package com.vz.backend.business.repository;

import com.vz.backend.business.domain.DocumentOutHistory;
import com.vz.backend.business.dto.DocumentOutHistoryDto;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.Pageable;
import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.data.jpa.repository.Query;
import org.springframework.stereotype.Repository;

@Repository
public interface IDocumentOutHistoryRepository extends JpaRepository<DocumentOutHistory, Long> {

    @Query(value = "select new com.vz.backend.business.dto.DocumentOutHistoryDto(d) from DocumentOutHistory d where d.docOutId = :docOutId and d.clientId = :clientId",
            countQuery = "select count (h) from DocumentOutHistory h where h.docOutId = :docOutId and h.clientId = :clientId")
    Page<DocumentOutHistoryDto> findByDocOutIdAndClientIdOrderByCreateDateDesc(Long docOutId, Pageable pageable, Long clientId);
}
