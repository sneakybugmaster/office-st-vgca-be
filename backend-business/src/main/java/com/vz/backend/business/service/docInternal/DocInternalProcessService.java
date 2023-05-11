package com.vz.backend.business.service.docInternal;

import com.vz.backend.business.domain.DocumentOutProcess;
import com.vz.backend.business.domain.documentInternal.DocInternalProcess;
import com.vz.backend.business.repository.IDocumentOutProcessRepository;
import com.vz.backend.business.repository.docInternal.IDocInternalProcessRepository;
import com.vz.backend.core.common.BussinessCommon;
import com.vz.backend.core.repository.IRepository;
import com.vz.backend.core.service.BaseService;
import lombok.extern.slf4j.Slf4j;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;

@Slf4j
@Service
public class DocInternalProcessService extends BaseService<DocInternalProcess> {

    @Override
    public IRepository<DocInternalProcess> getRepository() {
        return docInternalProcessRepository;
    }

    @Autowired
    IDocInternalProcessRepository docInternalProcessRepository;

    @Override
    public DocInternalProcess save(DocInternalProcess input) {
        log.error("input {}", input.getHandleType());
//        input.setOrgName(BussinessCommon.getUser().getOrgModel().getName()); // setOrg
        return getRepository().save(input);
    }

}
