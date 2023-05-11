package com.vz.backend.business.service.docInternal;

import java.util.ArrayList;
import java.util.List;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;

import com.vz.backend.business.config.DocInternalApproveTypeEnum;
import com.vz.backend.business.domain.documentInternal.DocInternalReceiver;
import com.vz.backend.business.dto.document.DocInternalReceiverDto;
import com.vz.backend.business.repository.docInternal.IDocInternalReceiverRepository;
import com.vz.backend.core.common.BussinessCommon;
import com.vz.backend.core.repository.IRepository;
import com.vz.backend.core.service.BaseService;

@Service
public class DocInternalReceiverService extends BaseService<DocInternalReceiver> {
    @Autowired
    IDocInternalReceiverRepository docInternalReceiverRepo;

    @Override
    public IRepository<DocInternalReceiver> getRepository() {
        return docInternalReceiverRepo;
    }

    public void add(Long docId, List<DocInternalReceiver> listReceiver) {
        List<DocInternalReceiver> result = new ArrayList<>();
        for (DocInternalReceiver receiver : listReceiver) {
            result.add(new DocInternalReceiver(docId, receiver.getType(), receiver.getUserId(), receiver.getOrgId(), receiver.getHandleStatus()));
        }
        docInternalReceiverRepo.saveAll(result);
    }

    public void update(Long docId, List<DocInternalReceiver> listReceiverInput) {
        List<DocInternalReceiver> listData = new ArrayList<>();
        List<DocInternalReceiver> listReceiver = docInternalReceiverRepo.findByDocIdAndClientId(docId, BussinessCommon.getClientId());
        if (listReceiver != null && !listReceiver.isEmpty()) {
            for (DocInternalReceiver element : listReceiver) {
                element.setActive(false);
            }
        }

        for (DocInternalReceiver receiver : listReceiverInput) {
            if (listReceiver != null && !listReceiver.isEmpty()) {
                for (int j = 0; j < listReceiver.size(); j++) {
                    if ((DocInternalApproveTypeEnum.ORG.equals(listReceiver.get(j).getType())
                            && listReceiver.get(j).getOrgId() != null
                            && listReceiver.get(j).getOrgId().equals(receiver.getOrgId()))
                            || (DocInternalApproveTypeEnum.USER.equals(listReceiver.get(j).getType())
                            && listReceiver.get(j).getUserId() != null
                            && listReceiver.get(j).getUserId().equals(receiver.getUserId()))) {
                        listReceiver.get(j).setActive(true);
                        listReceiver.get(j).setHandleStatus(receiver.getHandleStatus());
                        break;
                    }
                    // last item
                    if (j == listReceiver.size() - 1) {
                        listData.add(new DocInternalReceiver(docId, receiver.getType(), receiver.getUserId(),
                                receiver.getOrgId(), receiver.getHandleStatus()));
                    }
                }
            } else {
                listData.add(new DocInternalReceiver(docId, receiver.getType(), receiver.getUserId(),
                        receiver.getOrgId(), receiver.getHandleStatus()));
            }
        }
        docInternalReceiverRepo.saveAll(listData);
    }

    public List<DocInternalReceiverDto> getListReceiverByDocId(Long docId) {
        return docInternalReceiverRepo.getListReceiverByDocIdAndClientId(docId, BussinessCommon.getClientId());
    }

    public List<DocInternalReceiver> findByDocId(Long docId) {
        return docInternalReceiverRepo.findByDocIdAndClientIdAndActiveTrue(docId, BussinessCommon.getClientId());
    }

    public List<DocInternalReceiver> inActiveByDocId(Long docId) {
        List<DocInternalReceiver> docReiverList = findByDocId(docId);
        docReiverList.forEach(i -> i.setActive(false));
        return docInternalReceiverRepo.saveAll(docReiverList);
    }
}
