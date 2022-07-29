// This file only exists to typecheck that modules satisfy an interface
import * as shared_backend from './shared_backend';
import * as shared_user from './shared_user';
import {
  AnyBackendTy,
} from './shared_backend';
import {
  Stdlib_User_Shared,
  Stdlib_Backend_Shared,
} from './interfaces';

const _shared_backend: Stdlib_Backend_Shared<AnyBackendTy> = shared_backend;
void(_shared_backend);

const _shared_user: Stdlib_User_Shared = shared_user;
void(_shared_user);
